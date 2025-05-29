#include "compas.h"

struct Data {
    std::string name;
    int value;
    
    std::string to_string() {
        return "Data: " + name + " " + std::to_string(value);
    }
};

NB_MODULE(_class_primitives, m) {
    m.doc() = "Custom type example.";

    nb::class_<Data>(m, "Data")
        .def(nb::init<std::string>())
        .def(nb::init<const std::string&, int>())
        .def_rw("value", &Data::value)
        .def_rw("name", &Data::name)
        .def("to_string", &Data::to_string);

}
