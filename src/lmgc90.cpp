#include <nanobind/nanobind.h>
#include <nanobind/stl/vector.h>
#include <nanobind/stl/string.h>
#include <vector>
#include <string>
#include <memory>
#include <stdexcept>
#include <cmath>

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
    void lmgc90_apply_forces();
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

    // Visualization data for contact polygons and force vectors
    std::vector<std::vector<double>> interaction_normals;   // [3 values - normal vector N from uc]
    std::vector<std::vector<double>> interaction_tangent1;  // [3 values - tangent vector T from uc]
    std::vector<std::vector<double>> interaction_tangent2;  // [3 values - tangent vector S from uc]
    std::vector<std::vector<double>> interaction_force_global; // [3 values - force in global coords]
    std::vector<double> interaction_force_magnitude;        // scalar force magnitude

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

//╔═══════════════════════════════════════════════════════════════════════════╗
//║                        LMGC90 SOLVER WRAPPER CLASS                        ║
//╚═══════════════════════════════════════════════════════════════════════════╝

/**
 * @brief Wrapper class for LMGC90 solver with automatic memory management
 * @details Provides a C++ interface with unique_ptr pattern for safe resource handling
 */
class LMGC90Solver {
private:
    bool is_initialized;
    int nb_bodies_cached;
    std::vector<std::vector<double>> init_bodies;
    std::vector<std::vector<double>> init_body_frames;

    bool is_valid() const {
        return is_initialized;
    }

public:
    LMGC90Solver() : is_initialized(false), nb_bodies_cached(0) {}

    ~LMGC90Solver() {
        if (is_initialized) {
            lmgc90_finalize();
            is_initialized = false;
        }
    }

    //┌───────────────────────────────────────────────────────────────────────┐
    //│                    CORE SOLVER METHODS                                │
    //└───────────────────────────────────────────────────────────────────────┘

    void initialize(double dt, double theta) {
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
        if (!is_valid()) {
            throw std::runtime_error("Solver not initialized");
        }
        lmgc90_close_before_computing();
        nb_bodies_cached = lmgc90_get_nb_bodies();
        
        // Use unique_ptr for automatic memory management
        std::unique_ptr<lmgc90_rigid_body_3D[]> bodies(new lmgc90_rigid_body_3D[nb_bodies_cached]);
        lmgc90_get_all_bodies(bodies.get(), nb_bodies_cached);
        
        init_bodies.clear();
        init_body_frames.clear();
        for (int i = 0; i < nb_bodies_cached; i++) {
            init_bodies.push_back({bodies[i].coor[0], bodies[i].coor[1], bodies[i].coor[2]});
            init_body_frames.push_back({
                bodies[i].frame[0], bodies[i].frame[1], bodies[i].frame[2],
                bodies[i].frame[3], bodies[i].frame[4], bodies[i].frame[5],
                bodies[i].frame[6], bodies[i].frame[7], bodies[i].frame[8]
            });
        }
    }

    SimResult get_initial_state() {
        if (!is_valid()) {
            throw std::runtime_error("Solver not initialized");
        }
        
        std::unique_ptr<lmgc90_rigid_body_3D[]> bodies(new lmgc90_rigid_body_3D[nb_bodies_cached]);
        lmgc90_get_all_bodies(bodies.get(), nb_bodies_cached);
        
        SimResult result;
        for (int i = 0; i < nb_bodies_cached; i++) {
            result.bodies.push_back({bodies[i].coor[0], bodies[i].coor[1], bodies[i].coor[2]});
            result.body_frames.push_back({
                bodies[i].frame[0], bodies[i].frame[1], bodies[i].frame[2],
                bodies[i].frame[3], bodies[i].frame[4], bodies[i].frame[5],
                bodies[i].frame[6], bodies[i].frame[7], bodies[i].frame[8]
            });
        }
        
        result.init_bodies = init_bodies;
        result.init_body_frames = init_body_frames;
        return result;
    }

    SimResult compute_one_step() {
        if (!is_valid()) {
            throw std::runtime_error("Solver not initialized");
        }
        
        lmgc90_compute_one_step();
        
        std::unique_ptr<lmgc90_rigid_body_3D[]> bodies(new lmgc90_rigid_body_3D[nb_bodies_cached]);
        lmgc90_get_all_bodies(bodies.get(), nb_bodies_cached);
        
        int nb_inters = lmgc90_get_nb_inters();
        std::unique_ptr<lmgc90_inter_meca_3D[]> inters;
        if (nb_inters > 0) {
            inters.reset(new lmgc90_inter_meca_3D[nb_inters]);
            lmgc90_get_all_inters(inters.get(), nb_inters);
        }
        
        SimResult result;
        for (int i = 0; i < nb_bodies_cached; i++) {
            result.bodies.push_back({bodies[i].coor[0], bodies[i].coor[1], bodies[i].coor[2]});
            result.body_frames.push_back({
                bodies[i].frame[0], bodies[i].frame[1], bodies[i].frame[2],
                bodies[i].frame[3], bodies[i].frame[4], bodies[i].frame[5],
                bodies[i].frame[6], bodies[i].frame[7], bodies[i].frame[8]
            });
        }
        
        result.init_bodies = init_bodies;
        result.init_body_frames = init_body_frames;
        
        if (inters) {
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

                // Extract contact frame vectors for visualization
                // uc contains: [T1, T2, T3, N1, N2, N3, S1, S2, S3]
                double T[3] = {inters[i].uc[0], inters[i].uc[1], inters[i].uc[2]};
                double N[3] = {inters[i].uc[3], inters[i].uc[4], inters[i].uc[5]};
                double S[3] = {inters[i].uc[6], inters[i].uc[7], inters[i].uc[8]};
                
                result.interaction_tangent1.push_back({T[0], T[1], T[2]});
                result.interaction_normals.push_back({N[0], N[1], N[2]});
                result.interaction_tangent2.push_back({S[0], S[1], S[2]});
                
                // Transform local forces to global coordinates
                // rloc = [Ft, Fn, Fs] in local frame -> global = Ft*T + Fn*N + Fs*S
                double Ft = inters[i].rloc[0];
                double Fn = inters[i].rloc[1];
                double Fs = inters[i].rloc[2];
                
                double Fx = Ft * T[0] + Fn * N[0] + Fs * S[0];
                double Fy = Ft * T[1] + Fn * N[1] + Fs * S[1];
                double Fz = Ft * T[2] + Fn * N[2] + Fs * S[2];
                
                result.interaction_force_global.push_back({Fx, Fy, Fz});
                
                // Compute force magnitude
                double magnitude = std::sqrt(Fx*Fx + Fy*Fy + Fz*Fz);
                result.interaction_force_magnitude.push_back(magnitude);

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
                for (int k = 0; k < nint; ++k) {
                    internals.push_back(inters[i].internals[k]);
                }
                result.interaction_internals.push_back(internals);
            }
        }
        
        return result;
    }

    void finalize() {
        if (is_initialized) {
            lmgc90_finalize();
            is_initialized = false;
            nb_bodies_cached = 0;
            init_bodies.clear();
            init_body_frames.clear();
        }
    }
};

// Global solver instance for backwards compatibility with existing Python API
static std::unique_ptr<LMGC90Solver> g_solver;

//┌───────────────────────────────────────────────────────────────────────┐
//│                    SIMPLIFIED PYTHON API WRAPPERS                     │
//└───────────────────────────────────────────────────────────────────────┘

void initialize_simulation(double dt = 1e-3, double theta = 0.5) {
    if (!g_solver) {
        g_solver = std::make_unique<LMGC90Solver>();
    }
    g_solver->initialize(dt, theta);
}

void set_materials(int nb) {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    g_solver->set_materials(nb);
}

void set_tact_behavs(int nb) {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    g_solver->set_tact_behavs(nb);
}

void set_see_tables() {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    g_solver->set_see_tables();
}

void set_nb_bodies(int nb) {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    g_solver->set_nb_bodies(nb);
}

void set_one_polyr(std::vector<double> coor, std::vector<int> faces, std::vector<double> vertices, bool fixed) {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    g_solver->set_one_polyr(coor, faces, vertices, fixed);
}

void close_before_computing() {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    g_solver->close_before_computing();
}

SimResult get_initial_state() {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    return g_solver->get_initial_state();
}

SimResult compute_one_step() {
    if (!g_solver) throw std::runtime_error("Solver not initialized");
    return g_solver->compute_one_step();
}

void finalize_simulation() {
    if (g_solver) {
        g_solver->finalize();
        g_solver.reset();
    }
}


//╔═══════════════════════════════════════════════════════════════════════════╗
//║                        PYTHON MODULE BINDINGS                             ║
//╚═══════════════════════════════════════════════════════════════════════════╝

NB_MODULE(_lmgc90, m) {
    m.doc() = "LMGC90 DEM Solver - Minimal Python bindings for COMPAS integration";
    
    // LMGC90Solver class - main solver instance
    nb::class_<LMGC90Solver>(m, "LMGC90Solver")
        .def(nb::init<>(), "Create a new LMGC90 solver instance")
        .def("initialize", &LMGC90Solver::initialize, 
             nb::arg("dt"), nb::arg("theta"),
             "Initialize the solver")
        .def("set_materials", &LMGC90Solver::set_materials, nb::arg("nb"))
        .def("set_tact_behavs", &LMGC90Solver::set_tact_behavs, nb::arg("nb"))
        .def("set_see_tables", &LMGC90Solver::set_see_tables)
        .def("set_nb_bodies", &LMGC90Solver::set_nb_bodies, nb::arg("nb"))
        .def("set_one_polyr", &LMGC90Solver::set_one_polyr,
             nb::arg("coor"), nb::arg("faces"), nb::arg("vertices"), nb::arg("fixed") = false)
        .def("close_before_computing", &LMGC90Solver::close_before_computing)
        .def("get_initial_state", &LMGC90Solver::get_initial_state)
        .def("compute_one_step", &LMGC90Solver::compute_one_step)
        .def("finalize", &LMGC90Solver::finalize);
    
    // SimResult class - contains simulation state
    nb::class_<SimResult>(m, "SimResult")
        .def_rw("bodies", &SimResult::bodies, "Current body positions [x, y, z]")
        .def_rw("body_frames", &SimResult::body_frames, "Current body frames (3x3 rotation matrices)")
        .def_rw("init_bodies", &SimResult::init_bodies, "Initial body positions")
        .def_rw("init_body_frames", &SimResult::init_body_frames, "Initial body frames")
        .def_rw("interaction_coords", &SimResult::interaction_coords, "Contact points [x, y, z]")
        .def_rw("interaction_uc", &SimResult::interaction_uc, "Contact frames [T, N, S vectors]")
        .def_rw("interaction_bodies", &SimResult::interaction_bodies, "Interacting body pairs")
        .def_rw("interaction_rloc", &SimResult::interaction_rloc, "Local forces [Ft, Fn, Fs]")
        .def_rw("interaction_vloc", &SimResult::interaction_vloc, "Local velocities")
        .def_rw("interaction_gap", &SimResult::interaction_gap, "Contact gaps")
        .def_rw("interaction_status", &SimResult::interaction_status, "Contact status")
        // Visualization data for drawing contact polygons and force vectors
        .def_rw("interaction_normals", &SimResult::interaction_normals, "Contact normal vectors [Nx, Ny, Nz]")
        .def_rw("interaction_tangent1", &SimResult::interaction_tangent1, "Contact tangent vectors T [Tx, Ty, Tz]")
        .def_rw("interaction_tangent2", &SimResult::interaction_tangent2, "Contact tangent vectors S [Sx, Sy, Sz]")
        .def_rw("interaction_force_global", &SimResult::interaction_force_global, "Force vectors in global coords [Fx, Fy, Fz]")
        .def_rw("interaction_force_magnitude", &SimResult::interaction_force_magnitude, "Force magnitudes")
        // Metadata
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
    
    // Core solver functions - these are the only ones used by solver.py
    m.def("initialize_simulation", &initialize_simulation, 
          nb::arg("dt") = 1e-3, nb::arg("theta") = 0.5,
          "Initialize LMGC90 simulation");
    
    m.def("set_materials", &set_materials, nb::arg("nb"),
          "Set number of materials");
    
    m.def("set_tact_behavs", &set_tact_behavs, nb::arg("nb"),
          "Set number of contact behaviors");
    
    m.def("set_see_tables", &set_see_tables,
          "Configure contact detection tables");
    
    m.def("set_nb_bodies", &set_nb_bodies, nb::arg("nb"),
          "Set number of rigid bodies");
    
    m.def("set_one_polyr", &set_one_polyr, 
          nb::arg("coor"), nb::arg("faces"), nb::arg("vertices"), nb::arg("fixed") = false,
          "Add one polyhedral body");
    
    m.def("close_before_computing", &close_before_computing,
          "Finalize initialization before simulation");
    
    m.def("get_initial_state", &get_initial_state,
          "Get initial body state");
    
    m.def("compute_one_step", &compute_one_step,
          "Compute one simulation step");
    
    m.def("finalize_simulation", &finalize_simulation,
          "Cleanup and finalize simulation");
}
