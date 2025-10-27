#include <nanobind/nanobind.h>
#include <nanobind/stl/vector.h>
#include <nanobind/stl/string.h>
#include <vector>
#include <string>
#include <stdlib.h>

extern "C" {
    struct lmgc90_inter_meca_3D {
        char cdan[5]; int icdan; char cdbdy[5]; int icdbdy;
        char anbdy[5]; int ianbdy; char cdtac[5]; int icdtac;
        char antac[5]; int iantac; int icdsci; int iansci;
        char behav[5]; char status[5]; double coor[3]; double uc[9];
        double rloc[3]; double vloc[3]; double gap; int nb_int; double internals[19];
    };
    struct lmgc90_rigid_body_3D { double coor[3]; double frame[9]; };
    
    void lmgc90_initialize(double dt, double theta);
    void lmgc90_set_materials(int nb);
    void lmgc90_set_tact_behavs(int nb);
    void lmgc90_set_see_tables(void);
    void lmgc90_set_nb_bodies(int nb);
    void lmgc90_set_one_polyr(double coor[3], int* faces, int nb_faces, double* vertices, int nb_v, bool fixed);
    void lmgc90_close_before_computing(void);
    void lmgc90_compute_one_step(void);
    void lmgc90_finalize(void);
    int lmgc90_get_nb_inters();
    void lmgc90_get_all_inters(lmgc90_inter_meca_3D* inters, int size);
    int lmgc90_get_nb_bodies();
    void lmgc90_get_all_bodies(lmgc90_rigid_body_3D* bodies, int size);
}

namespace nb = nanobind;

struct SimResult {
    std::vector<std::vector<double>> bodies;           // [x, y, z]
    std::vector<std::vector<double>> body_frames;       // [9 values - rotation matrix]
    std::vector<std::vector<double>> init_bodies;       // initial [x, y, z]
    std::vector<std::vector<double>> init_body_frames;  // initial [9 values]
    std::vector<std::vector<double>> interaction_coords; // [x, y, z]
    std::vector<std::vector<double>> interaction_uc;    // [9 values - contact frame T,N,S]
    std::vector<std::vector<int>> interaction_bodies;   // [icdbdy, ianbdy]
    std::vector<std::vector<double>> interaction_rloc;  // [3 values - local forces]
    std::vector<std::vector<double>> interaction_vloc;  // [3 values]
    std::vector<double> interaction_gap;
    std::vector<std::string> interaction_status;

    // Inputs (hardcoded example) transferred to Python as-is
    std::vector<int> faces_input;                       // faces (1-indexed)
    std::vector<std::vector<double>> vertices_input;    // per-body vertices (local)
    std::vector<std::vector<double>> coors_input;       // per-body initial centers (world)

    // Full interaction metadata (mirrors lmgc90_inter_meca_3D)
    std::vector<std::string> interaction_cdan;   // contacting geometry code
    std::vector<int>         interaction_icdan;
    std::vector<std::string> interaction_cdbdy;  // contacting body code
    std::vector<int>         interaction_icdsci; // contacting subcomponent index
    std::vector<std::string> interaction_anbdy;  // contacted body code
    std::vector<int>         interaction_ianbdy;
    std::vector<std::string> interaction_cdtac;  // contacting tact code
    std::vector<int>         interaction_icdtac;
    std::vector<std::string> interaction_antac;  // contacted tact code
    std::vector<int>         interaction_iantac;
    std::vector<std::string> interaction_behav;  // behavior code
    std::vector<int>         interaction_nb_int; // number of internal values
    std::vector<std::vector<double>> interaction_internals; // internal values per interaction
};

// Global state management
static bool is_initialized = false;
static int nb_bodies_cached = 0;

void initialize_simulation(double dt, double theta) {
    if (!is_initialized) {
        lmgc90_initialize(dt, theta);
        is_initialized = true;
    }
}

void set_materials(int nb) {
    lmgc90_set_materials(nb);
}

void set_tact_behavs(int nb) {
    lmgc90_set_tact_behavs(nb);
}

void set_see_tables() {
    lmgc90_set_see_tables();
}

void set_nb_bodies(int nb) {
    lmgc90_set_nb_bodies(nb);
}

void set_one_polyr(std::vector<double> coor, std::vector<int> faces, std::vector<double> vertices, bool fixed) {
    if (coor.size() != 3) {
        throw std::runtime_error("coor must have 3 elements [x, y, z]");
    }
    if (faces.size() % 3 != 0) {
        throw std::runtime_error("faces must be divisible by 3 (triangular faces)");
    }
    if (vertices.size() % 3 != 0) {
        throw std::runtime_error("vertices must be divisible by 3 (x, y, z coordinates)");
    }
    
    int nb_faces = faces.size() / 3;
    int nb_vertices = vertices.size() / 3;
    
    lmgc90_set_one_polyr(coor.data(), faces.data(), nb_faces, vertices.data(), nb_vertices, fixed);
}

void close_before_computing() {
    if (!is_initialized) {
        throw std::runtime_error("Simulation not initialized. Call initialize_simulation() first.");
    }
    lmgc90_close_before_computing();
    nb_bodies_cached = lmgc90_get_nb_bodies();
}

SimResult get_initial_state() {
    if (!is_initialized) {
        throw std::runtime_error("Simulation not initialized. Call initialize_simulation() first.");
    }
    
    // Get initial state without computing a step
    lmgc90_rigid_body_3D* bodies = (lmgc90_rigid_body_3D*)malloc(nb_bodies_cached * sizeof(lmgc90_rigid_body_3D));
    lmgc90_get_all_bodies(bodies, nb_bodies_cached);
    
    SimResult result;
    
    // Copy bodies (no interactions yet)
    for (int i = 0; i < nb_bodies_cached; i++) {
        result.bodies.push_back({bodies[i].coor[0], bodies[i].coor[1], bodies[i].coor[2]});
        result.body_frames.push_back({
            bodies[i].frame[0], bodies[i].frame[1], bodies[i].frame[2],
            bodies[i].frame[3], bodies[i].frame[4], bodies[i].frame[5],
            bodies[i].frame[6], bodies[i].frame[7], bodies[i].frame[8]
        });
    }
    
    free(bodies);
    return result;
}

SimResult compute_one_step() {
    if (!is_initialized) {
        throw std::runtime_error("Simulation not initialized. Call initialize_simulation() first.");
    }
    
    lmgc90_compute_one_step();
    
    // Get current state
    lmgc90_rigid_body_3D* bodies = (lmgc90_rigid_body_3D*)malloc(nb_bodies_cached * sizeof(lmgc90_rigid_body_3D));
    lmgc90_get_all_bodies(bodies, nb_bodies_cached);
    
    int nb_inters = lmgc90_get_nb_inters();
    lmgc90_inter_meca_3D* inters = nullptr;
    if (nb_inters > 0) {
        inters = (lmgc90_inter_meca_3D*)malloc(nb_inters * sizeof(lmgc90_inter_meca_3D));
        lmgc90_get_all_inters(inters, nb_inters);
    }
    
    SimResult result;
    
    // Copy bodies
    for (int i = 0; i < nb_bodies_cached; i++) {
        result.bodies.push_back({bodies[i].coor[0], bodies[i].coor[1], bodies[i].coor[2]});
        result.body_frames.push_back({
            bodies[i].frame[0], bodies[i].frame[1], bodies[i].frame[2],
            bodies[i].frame[3], bodies[i].frame[4], bodies[i].frame[5],
            bodies[i].frame[6], bodies[i].frame[7], bodies[i].frame[8]
        });
    }
    
    // Copy interactions
    for (int i = 0; i < nb_inters; i++) {
        // Basic geometry/frames/forces
        result.interaction_coords.push_back({inters[i].coor[0], inters[i].coor[1], inters[i].coor[2]});
        result.interaction_uc.push_back({
            inters[i].uc[0], inters[i].uc[1], inters[i].uc[2],
            inters[i].uc[3], inters[i].uc[4], inters[i].uc[5],
            inters[i].uc[6], inters[i].uc[7], inters[i].uc[8]
        });
        result.interaction_bodies.push_back({inters[i].icdbdy, inters[i].ianbdy});
        result.interaction_rloc.push_back({inters[i].rloc[0], inters[i].rloc[1], inters[i].rloc[2]});
        result.interaction_vloc.push_back({inters[i].vloc[0], inters[i].vloc[1], inters[i].vloc[2]});
        result.interaction_gap.push_back(inters[i].gap);
        result.interaction_status.push_back(std::string(inters[i].status, 4));

        // Metadata
        result.interaction_cdan.push_back(std::string(inters[i].cdan, 4));
        result.interaction_icdan.push_back(inters[i].icdan);
        result.interaction_cdbdy.push_back(std::string(inters[i].cdbdy, 4));
        result.interaction_icdsci.push_back(inters[i].icdsci);
        result.interaction_anbdy.push_back(std::string(inters[i].anbdy, 4));
        result.interaction_ianbdy.push_back(inters[i].ianbdy);
        result.interaction_cdtac.push_back(std::string(inters[i].cdtac, 4));
        result.interaction_icdtac.push_back(inters[i].icdtac);
        result.interaction_antac.push_back(std::string(inters[i].antac, 4));
        result.interaction_iantac.push_back(inters[i].iantac);
        result.interaction_behav.push_back(std::string(inters[i].behav, 4));
        result.interaction_nb_int.push_back(inters[i].nb_int);
        std::vector<double> internals;
        int nint = inters[i].nb_int;
        if (nint < 0) nint = 0;
        if (nint > 19) nint = 19;
        for (int k = 0; k < nint; ++k) internals.push_back(inters[i].internals[k]);
        result.interaction_internals.push_back(internals);
    }
    
    free(bodies);
    if (inters) free(inters);
    return result;
}

void finalize_simulation() {
    if (is_initialized) {
        lmgc90_finalize();
        is_initialized = false;
        nb_bodies_cached = 0;
    }
}

// Initialize with hardcoded geometry and run nb_steps (NO interaction retrieval). Return
// inputs + initial bodies/frames and final bodies/frames.
SimResult run_hardcoded_bodies(int nb_steps, double dt, double theta) {
    // Geometry from comgc90.c
    std::vector<int> faces = {1, 2, 3, 1, 2, 4, 2, 3, 4, 3, 1, 4};
    std::vector<double> vert_base = {0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.3, 0.3, 1.0};
    std::vector<double> coor_base = {0.0, 0.0, 1.0};

    std::vector<double> vert1 = vert_base;
    std::vector<double> coor1 = coor_base;
    std::vector<double> vert2 = vert_base; vert2[11] = -1.0;      // flip apex
    std::vector<double> coor2 = coor_base; coor2[2] -= 1.e-2;      // lower

    // First part of initialize
    initialize_simulation(dt, theta);
    set_materials(1);
    set_tact_behavs(1);
    set_see_tables();
    set_nb_bodies(2);

    set_one_polyr(coor1, faces, vert1, false);
    set_one_polyr(coor2, faces, vert2, true);

    // Finish initialization and cache number of bodies
    close_before_computing();

    SimResult result;
    result.faces_input = faces;
    result.vertices_input.push_back(vert1);
    result.vertices_input.push_back(vert2);
    result.coors_input.push_back(coor1);
    result.coors_input.push_back(coor2);

    // Initial bodies/frames (after close_before_computing)
    lmgc90_rigid_body_3D* bodies = (lmgc90_rigid_body_3D*)malloc(nb_bodies_cached * sizeof(lmgc90_rigid_body_3D));
    lmgc90_get_all_bodies(bodies, nb_bodies_cached);
    for (int i = 0; i < nb_bodies_cached; ++i) {
        result.init_bodies.push_back({bodies[i].coor[0], bodies[i].coor[1], bodies[i].coor[2]});
        result.init_body_frames.push_back({
            bodies[i].frame[0], bodies[i].frame[1], bodies[i].frame[2],
            bodies[i].frame[3], bodies[i].frame[4], bodies[i].frame[5],
            bodies[i].frame[6], bodies[i].frame[7], bodies[i].frame[8]
        });
    }

    // Time loop (no interaction retrieval to avoid segfaults)
    for (int i_step = 0; i_step < nb_steps; ++i_step) {
        lmgc90_compute_one_step();
    }

    // Final bodies/frames
    lmgc90_get_all_bodies(bodies, nb_bodies_cached);
    for (int i = 0; i < nb_bodies_cached; ++i) {
        result.bodies.push_back({bodies[i].coor[0], bodies[i].coor[1], bodies[i].coor[2]});
        result.body_frames.push_back({
            bodies[i].frame[0], bodies[i].frame[1], bodies[i].frame[2],
            bodies[i].frame[3], bodies[i].frame[4], bodies[i].frame[5],
            bodies[i].frame[6], bodies[i].frame[7], bodies[i].frame[8]
        });
    }

    free(bodies);
    finalize_simulation();
    return result;
}
// Initialize with hardcoded geometry and run nb_steps, returning last step state
SimResult run_hardcoded(int nb_steps, double dt, double theta) {
    // Geometry from comgc90.c
    std::vector<int> faces = {1, 2, 3, 1, 2, 4, 2, 3, 4, 3, 1, 4};
    std::vector<double> vert_base = {0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.3, 0.3, 1.0};
    std::vector<double> coor_base = {0.0, 0.0, 1.0};

    std::vector<double> vert1 = vert_base;
    std::vector<double> coor1 = coor_base;
    std::vector<double> vert2 = vert_base; vert2[11] = -1.0;      // flip apex
    std::vector<double> coor2 = coor_base; coor2[2] -= 1.e-2;      // lower

    // First part of initialize
    initialize_simulation(dt, theta);
    set_materials(1);
    set_tact_behavs(1);
    set_see_tables();
    set_nb_bodies(2);

    set_one_polyr(coor1, faces, vert1, false);
    set_one_polyr(coor2, faces, vert2, true);

    // Finish initialization
    close_before_computing();

    // Time loop
    SimResult last;
    for (int i_step = 0; i_step < nb_steps; ++i_step) {
        last = compute_one_step();
    }

    finalize_simulation();
    return last;
}
// Get INPUT geometry (from comgc90.c) - NO simulation
SimResult get_hardcoded_geometry(double dt, double theta) {
    // Define pyramid geometry (from comgc90.c)
    std::vector<int> faces = {1, 2, 3, 1, 2, 4, 2, 3, 4, 3, 1, 4};
    std::vector<double> vert_base = {0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.3, 0.3, 1.0};
    std::vector<double> coor_base = {0.0, 0.0, 1.0}; // EXACT comgc90.c

    // Prepare per-body inputs exactly as comgc90.c
    std::vector<double> vert1 = vert_base;
    std::vector<double> coor1 = coor_base;
    std::vector<double> vert2 = vert_base; vert2[11] = -1.0; // flip apex
    std::vector<double> coor2 = coor_base; coor2[2] -= 1.e-2; // lower by 1e-2

    // Initialize (first part only)
    initialize_simulation(dt, theta);
    set_materials(1);
    set_tact_behavs(1);
    set_see_tables();
    set_nb_bodies(2);

    // Bodies
    set_one_polyr(coor1, faces, vert1, false);
    set_one_polyr(coor2, faces, vert2, true);

    // Finish initialization
    close_before_computing();

    // Package result: inputs + initial state (no compute_one_step)
    SimResult result = get_initial_state();
    result.faces_input = faces;
    result.vertices_input.push_back(vert1);
    result.vertices_input.push_back(vert2);
    result.coors_input.push_back(coor1);
    result.coors_input.push_back(coor2);

    finalize_simulation();
    return result;
}

NB_MODULE(_lmgc90, m) {
    nb::class_<SimResult>(m, "SimResult")
        .def_rw("bodies", &SimResult::bodies)
        .def_rw("body_frames", &SimResult::body_frames)
        .def_rw("init_bodies", &SimResult::init_bodies)
        .def_rw("init_body_frames", &SimResult::init_body_frames)
        .def_rw("interaction_coords", &SimResult::interaction_coords)
        .def_rw("interaction_uc", &SimResult::interaction_uc)
        .def_rw("interaction_bodies", &SimResult::interaction_bodies)
        .def_rw("interaction_rloc", &SimResult::interaction_rloc)
        .def_rw("interaction_vloc", &SimResult::interaction_vloc)
        .def_rw("interaction_gap", &SimResult::interaction_gap)
        .def_rw("interaction_status", &SimResult::interaction_status)
        // Inputs
        .def_rw("faces_input", &SimResult::faces_input)
        .def_rw("vertices_input", &SimResult::vertices_input)
        .def_rw("coors_input", &SimResult::coors_input)
        // Full interaction metadata
        .def_rw("interaction_cdan", &SimResult::interaction_cdan)
        .def_rw("interaction_icdan", &SimResult::interaction_icdan)
        .def_rw("interaction_cdbdy", &SimResult::interaction_cdbdy)
        .def_rw("interaction_icdsci", &SimResult::interaction_icdsci)
        .def_rw("interaction_anbdy", &SimResult::interaction_anbdy)
        .def_rw("interaction_ianbdy", &SimResult::interaction_ianbdy)
        .def_rw("interaction_cdtac", &SimResult::interaction_cdtac)
        .def_rw("interaction_icdtac", &SimResult::interaction_icdtac)
        .def_rw("interaction_antac", &SimResult::interaction_antac)
        .def_rw("interaction_iantac", &SimResult::interaction_iantac)
        .def_rw("interaction_behav", &SimResult::interaction_behav)
        .def_rw("interaction_nb_int", &SimResult::interaction_nb_int)
        .def_rw("interaction_internals", &SimResult::interaction_internals);
    
    m.def("initialize_simulation", &initialize_simulation, 
          nb::arg("dt") = 1e-3, nb::arg("theta") = 0.5,
          "Initialize LMGC90 simulation with time step and theta parameter");
    
    m.def("set_materials", &set_materials, 
          nb::arg("nb"),
          "Set number of materials");
    
    m.def("set_tact_behavs", &set_tact_behavs, 
          nb::arg("nb"),
          "Set number of tactical behaviors");
    
    m.def("set_see_tables", &set_see_tables, 
          "Set see tables");
    
    m.def("set_nb_bodies", &set_nb_bodies, 
          nb::arg("nb"),
          "Set number of bodies");
    
    m.def("set_one_polyr", &set_one_polyr, 
          nb::arg("coor"), nb::arg("faces"), nb::arg("vertices"), nb::arg("fixed") = false,
          "Set one polyhedron body. coor=[x,y,z], faces=[v1,v2,v3,...] (1-indexed), vertices=[x1,y1,z1,x2,y2,z2,...]");
    
    m.def("close_before_computing", &close_before_computing, 
          "Close initialization and prepare for computation");
    
    m.def("get_initial_state", &get_initial_state, 
          "Get initial body positions and orientations after initialization (without running simulation)");
    
    m.def("compute_one_step", &compute_one_step, 
          "Compute one simulation step and return current state");
    
    m.def("finalize_simulation", &finalize_simulation, 
          "Finalize and cleanup LMGC90 simulation");
    
    m.def("get_hardcoded_geometry", &get_hardcoded_geometry, 
          nb::arg("dt") = 1e-3, nb::arg("theta") = 0.5,
          "Get hardcoded geometry from comgc90.c (initial state only, no simulation)");

    m.def("run_hardcoded", &run_hardcoded,
          nb::arg("nb_steps") = 1, nb::arg("dt") = 1e-3, nb::arg("theta") = 0.5,
          "Initialize with hardcoded geometry and run nb_steps, returning last step state");

    m.def("run_hardcoded_bodies", &run_hardcoded_bodies,
          nb::arg("nb_steps") = 100, nb::arg("dt") = 1e-3, nb::arg("theta") = 0.5,
          "Initialize with hardcoded geometry and run nb_steps; returns inputs, initial and final bodies/frames (no interactions)");
}

