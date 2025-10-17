#!/usr/bin/env python3
"""
Dependency checker for compas_lmgc90 project.
This script checks for all required and optional dependencies.
"""

import os
import sys
import subprocess
import shutil
from pathlib import Path
from typing import Dict, List, Tuple, Optional

class Colors:
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    PURPLE = '\033[0;35m'
    CYAN = '\033[0;36m'
    NC = '\033[0m'  # No Color

class DependencyChecker:
    def __init__(self):
        self.project_root = Path(__file__).parent
        self.results = {
            'required': {},
            'optional': {},
            'python_packages': {},
            'paths': {}
        }
        
    def print_status(self, message: str, status: str = "INFO"):
        colors = {
            "INFO": Colors.BLUE,
            "SUCCESS": Colors.GREEN,
            "WARNING": Colors.YELLOW,
            "ERROR": Colors.RED,
            "FOUND": Colors.GREEN,
            "MISSING": Colors.RED
        }
        color = colors.get(status, Colors.NC)
        print(f"{color}[{status}]{Colors.NC} {message}")

    def run_command(self, cmd: List[str], capture_output: bool = True) -> Tuple[bool, str]:
        """Run a command and return success status and output."""
        try:
            result = subprocess.run(
                cmd, 
                capture_output=capture_output, 
                text=True, 
                timeout=10
            )
            return result.returncode == 0, result.stdout.strip()
        except (subprocess.TimeoutExpired, FileNotFoundError, subprocess.SubprocessError):
            return False, ""

    def check_command(self, command: str, name: str = None) -> bool:
        """Check if a command is available."""
        name = name or command
        if shutil.which(command):
            success, output = self.run_command([command, '--version'])
            if success:
                # Extract version info (first line usually contains version)
                version = output.split('\n')[0] if output else "unknown version"
                self.print_status(f"{name}: {version}", "FOUND")
                return True
            else:
                self.print_status(f"{name}: found but version check failed", "WARNING")
                return True
        else:
            self.print_status(f"{name}: not found", "MISSING")
            return False

    def check_python_package(self, package: str, import_name: str = None) -> bool:
        """Check if a Python package is installed."""
        import_name = import_name or package
        try:
            __import__(import_name)
            # Try to get version
            try:
                import importlib.metadata
                version = importlib.metadata.version(package)
                self.print_status(f"Python package {package}: {version}", "FOUND")
            except:
                self.print_status(f"Python package {package}: installed", "FOUND")
            return True
        except ImportError:
            self.print_status(f"Python package {package}: not installed", "MISSING")
            return False

    def check_file_path(self, path: str, description: str) -> bool:
        """Check if a file or directory exists."""
        if os.path.exists(path):
            self.print_status(f"{description}: {path}", "FOUND")
            return True
        else:
            self.print_status(f"{description}: {path} (not found)", "MISSING")
            return False

    def check_intel_oneapi(self) -> Dict[str, bool]:
        """Check Intel OneAPI installation."""
        results = {}
        
        # Check OneAPI setvars script
        oneapi_script = "/opt/intel/oneapi/setvars.sh"
        results['setvars'] = self.check_file_path(oneapi_script, "Intel OneAPI setvars script")
        
        # Check for Intel compilers
        results['ifort'] = self.check_command('ifort', 'Intel Fortran compiler')
        results['icx'] = self.check_command('icx', 'Intel C++ compiler')
        results['icc'] = self.check_command('icc', 'Intel C compiler (legacy)')
        
        # Check MKL
        mkl_root = os.environ.get('MKLROOT')
        if mkl_root:
            results['mkl'] = self.check_file_path(mkl_root, "Intel MKL")
        else:
            # Common MKL locations
            mkl_paths = [
                "/opt/intel/oneapi/mkl/latest",
                "/opt/intel/mkl",
                "/usr/local/intel/mkl"
            ]
            results['mkl'] = any(self.check_file_path(path, "Intel MKL") for path in mkl_paths)
        
        return results

    def check_lmgc90_build(self) -> Dict[str, bool]:
        """Check LMGC90 build artifacts."""
        results = {}
        
        # Default paths from CMakeLists.txt
        lib_path = self.project_root / "src/lmgc90_dev/build_core_min_oneapi/lib"
        include_path = self.project_root / "src/lmgc90_dev/build_core_min_oneapi/modules"
        
        results['lib_dir'] = self.check_file_path(str(lib_path), "LMGC90 library directory")
        results['include_dir'] = self.check_file_path(str(include_path), "LMGC90 include directory")
        
        # Check for some key libraries
        if lib_path.exists():
            key_libs = [
                "liblmgc_core_shared.so",
                "liblmgc_core_rigid_3d.so", 
                "liblmgc_core_contact_3d.so"
            ]
            for lib in key_libs:
                lib_file = lib_path / lib
                results[f'lib_{lib}'] = self.check_file_path(str(lib_file), f"LMGC90 {lib}")
        
        return results

    def check_system_libraries(self) -> Dict[str, bool]:
        """Check system libraries."""
        results = {}
        
        # Check for alternative BLAS implementations
        results['openblas'] = self.check_command('pkg-config', 'pkg-config') and \
                             self.run_command(['pkg-config', '--exists', 'openblas'])[0]
        
        if not results['openblas']:
            # Try to find OpenBLAS library files
            openblas_paths = [
                "/usr/lib/x86_64-linux-gnu/libopenblas.so",
                "/usr/lib/libopenblas.so",
                "/usr/local/lib/libopenblas.so"
            ]
            results['openblas'] = any(os.path.exists(path) for path in openblas_paths)
        
        if results['openblas']:
            self.print_status("OpenBLAS: found", "FOUND")
        else:
            self.print_status("OpenBLAS: not found", "MISSING")
        
        # Check gfortran
        results['gfortran'] = self.check_command('gfortran', 'GNU Fortran compiler')
        
        return results

    def run_full_check(self):
        """Run complete dependency check."""
        print(f"{Colors.CYAN}{'='*60}")
        print(f"COMPAS LMGC90 Dependency Checker")
        print(f"{'='*60}{Colors.NC}")
        
        # Basic build tools
        print(f"\n{Colors.PURPLE}=== Build Tools ==={Colors.NC}")
        self.results['required']['cmake'] = self.check_command('cmake')
        self.results['required']['make'] = self.check_command('make')
        self.results['required']['gcc'] = self.check_command('gcc', 'GCC C compiler')
        self.results['required']['g++'] = self.check_command('g++', 'GCC C++ compiler')
        
        # Python and packages
        print(f"\n{Colors.PURPLE}=== Python Environment ==={Colors.NC}")
        self.results['required']['python'] = self.check_command('python3', 'Python 3')
        self.results['required']['pip'] = self.check_command('pip', 'pip')
        
        # Required Python packages
        required_packages = ['numpy', 'nanobind']
        for package in required_packages:
            self.results['python_packages'][package] = self.check_python_package(package)
        
        # Optional Python packages
        optional_packages = ['compas', 'compas_dem', 'compas_viewer']
        for package in optional_packages:
            self.results['python_packages'][package] = self.check_python_package(package)
        
        # Intel OneAPI
        print(f"\n{Colors.PURPLE}=== Intel OneAPI ==={Colors.NC}")
        intel_results = self.check_intel_oneapi()
        self.results['optional'].update(intel_results)
        
        # System libraries
        print(f"\n{Colors.PURPLE}=== System Libraries ==={Colors.NC}")
        sys_results = self.check_system_libraries()
        self.results['optional'].update(sys_results)
        
        # LMGC90 build artifacts
        print(f"\n{Colors.PURPLE}=== LMGC90 Build Artifacts ==={Colors.NC}")
        lmgc_results = self.check_lmgc90_build()
        self.results['paths'].update(lmgc_results)
        
        # Summary
        self.print_summary()

    def print_summary(self):
        """Print dependency check summary."""
        print(f"\n{Colors.CYAN}{'='*60}")
        print(f"SUMMARY")
        print(f"{'='*60}{Colors.NC}")
        
        # Count results
        required_missing = sum(1 for v in self.results['required'].values() if not v)
        python_missing = sum(1 for k, v in self.results['python_packages'].items() 
                           if not v and k in ['numpy', 'nanobind'])
        
        if required_missing == 0 and python_missing == 0:
            self.print_status("All required dependencies are available!", "SUCCESS")
        else:
            self.print_status(f"Missing {required_missing + python_missing} required dependencies", "ERROR")
        
        # Intel OneAPI status
        intel_available = any(self.results['optional'].get(k, False) 
                            for k in ['ifort', 'icx', 'mkl'])
        if intel_available:
            self.print_status("Intel OneAPI components detected", "SUCCESS")
        else:
            self.print_status("Intel OneAPI not detected - will use system compilers", "WARNING")
        
        # LMGC90 build status
        lmgc_built = self.results['paths'].get('lib_dir', False)
        if lmgc_built:
            self.print_status("LMGC90 libraries found", "SUCCESS")
        else:
            self.print_status("LMGC90 libraries not found - need to build first", "WARNING")
        
        # Recommendations
        print(f"\n{Colors.PURPLE}=== Recommendations ==={Colors.NC}")
        
        if required_missing > 0:
            self.print_status("Install missing build tools with: sudo apt install build-essential cmake", "INFO")
        
        if python_missing > 0:
            self.print_status("Install missing Python packages with: pip install numpy nanobind", "INFO")
        
        if not lmgc_built:
            self.print_status("Build LMGC90 libraries first before installing compas_lmgc90", "INFO")
        
        if not intel_available:
            self.print_status("Consider installing Intel OneAPI for better performance", "INFO")

if __name__ == "__main__":
    checker = DependencyChecker()
    checker.run_full_check()
