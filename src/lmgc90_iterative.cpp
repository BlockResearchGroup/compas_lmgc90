#include <nanobind/nanobind.h>
#include <nanobind/stl/vector.h>
#include <nanobind/stl/string.h>
#include <vector>
#include <string>
#include <stdlib.h>
#include <stdexcept>

extern "C" {
    struct lmgc90_inter_meca_3D {
        char cdan[5]; int icdan; char cdbdy[5]; int icdbdy;
        char anbdy[5]; int ianbdy; char cdtac[5]; int icdtac;
        char antac[5]; int iantac; int icdsci; int iansci;
        char behav[5]; char status[5]; double coor[3]; double uc[9];
        double rloc[3]; double vloc[3]; double gap; int nb_int; double internals[19];
    };
    struct lmgc90_rigid_body_3D { double coor[3]; double frame[9]; };
    
    void lmgc90_initialize(void);
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
    std::vector<std::vector<double>> interaction_coords; // [x, y, z]
    std::vector<std::vector<double>> interaction_uc;    // [9 values - contact frame T,N,S]
    std::vector<std::vector<int>> interaction_bodies;   // [icdbdy, ianbdy]
    std::vector<std::vector<double>> interaction_rloc;  // [3 values - local forces]
    std::vector<std::vector<double>> interaction_vloc;  // [3 values]
    std::vector<double> interaction_gap;
    std::vector<std::string> interaction_status;
};

// Global state management
static bool is_initialized = false;
static int nb_bodies_cached = 0;

void initialize_simulation() {
    if (!is_initialized) {
        lmgc90_initialize();
        nb_bodies_cached = lmgc90_get_nb_bodies();
        is_initialized = true;
    }
}

SimResult compute_one_step() {
    if (!is_initialized) {
        throw std::runtime_error("Simulation not initialized. Call initialize_simulation() first.");
    }
    
    for(int i = 0; i< 5; i++)
        lmgc90_compute_one_step();
    
    // Get current state
    lmgc90_rigid_body_3D* bodies = (lmgc90_rigid_body_3D*)malloc(nb_bodies_cached * sizeof(lmgc90_rigid_body_3D));
    lmgc90_get_all_bodies(bodies, nb_bodies_cached);
    
    int nb_inters = lmgc90_get_nb_inters();
    lmgc90_inter_meca_3D* inters = (lmgc90_inter_meca_3D*)malloc(nb_inters * sizeof(lmgc90_inter_meca_3D));
    lmgc90_get_all_inters(inters, nb_inters);
    
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
    }
    
    free(bodies); free(inters);
    return result;
}

void finalize_simulation() {
    if (is_initialized) {
        lmgc90_finalize();
        is_initialized = false;
        nb_bodies_cached = 0;
    }
}

bool is_simulation_initialized() {
    return is_initialized;
}

int get_nb_bodies() {
    if (!is_initialized) {
        throw std::runtime_error("Simulation not initialized. Call initialize_simulation() first.");
    }
    return nb_bodies_cached;
}

// Legacy function for backward compatibility
SimResult run_simulation() {
    initialize_simulation();
    SimResult result = compute_one_step();
    finalize_simulation();
    return result;
}

NB_MODULE(_lmgc90_iterative, m) {
    nb::class_<SimResult>(m, "SimResult")
        .def_rw("bodies", &SimResult::bodies)
        .def_rw("body_frames", &SimResult::body_frames)
        .def_rw("interaction_coords", &SimResult::interaction_coords)
        .def_rw("interaction_uc", &SimResult::interaction_uc)
        .def_rw("interaction_bodies", &SimResult::interaction_bodies)
        .def_rw("interaction_rloc", &SimResult::interaction_rloc)
        .def_rw("interaction_vloc", &SimResult::interaction_vloc)
        .def_rw("interaction_gap", &SimResult::interaction_gap)
        .def_rw("interaction_status", &SimResult::interaction_status);
    
    m.def("initialize_simulation", &initialize_simulation, 
          "Initialize LMGC90 simulation");
    
    m.def("compute_one_step", &compute_one_step, 
          "Compute one simulation step and return current state");
    
    m.def("finalize_simulation", &finalize_simulation, 
          "Finalize and cleanup LMGC90 simulation");
    
    m.def("is_simulation_initialized", &is_simulation_initialized, 
          "Check if simulation is initialized");
    
    m.def("get_nb_bodies", &get_nb_bodies, 
          "Get number of bodies in the simulation");
    
    m.def("run_simulation", &run_simulation, 
          "Run LMGC90 simulation and return result with bodies and interactions (legacy)");
}
