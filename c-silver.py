#!/usr/bin/env python3
"""
C Macro Schema to Silver Format Converter

Converts from C macro-based schema definitions to readable Silver format.
"""

import re
import sys
from typing import List, Dict, Tuple, Optional

class SilverConverter:
    def __init__(self):
        self.class_patterns = {
            'schema': re.compile(r'#define\s+(\w+)_schema\(X,Y,\.\.\.\)'),
            'declare_base': re.compile(r'declare_base\((\w+)(?:,\s*([^)]*))?\)'),
            'declare_base_meta': re.compile(r'declare_base_meta\((\w+)(?:,\s*([^)]*))?\)'),
            'declare_primitive': re.compile(r'declare_primitive\((\w+)(?:,\s*([^)]*))?\)'),
            'declare_abstract': re.compile(r'declare_abstract\((\w+)(?:,\s*([^)]*))?\)'),
            'declare_class': re.compile(r'declare_class\((\w+)(?:,\s*([^)]*))?\)'),
            'declare_class_2': re.compile(r'declare_class_2\((\w+),\s*(\w+)(?:,\s*(\w+))?(?:,\s*([^)]*))?\)'),
            'declare_class_3': re.compile(r'declare_class_3\((\w+),\s*(\w+),\s*(\w+)(?:,\s*([^)]*))?\)'),
            'declare_struct': re.compile(r'declare_struct\((\w+)(?:,\s*([^)]*))?\)'),
            'declare_enum': re.compile(r'declare_enum\((\w+)(?:,\s*([^)]*))?\)'),
            'declare_typed_enum': re.compile(r'declare_typed_enum\((\w+),\s*(\w+)(?:,\s*([^)]*))?\)'),
        }
        
        self.property_patterns = {
            'i_prop': re.compile(r'i_prop\s*\(X,Y,\s*(\w+),\s*([^,]+),\s*(\w+)(?:,\s*([^)]+))?\)'),
            'i_struct_prop': re.compile(r'i_struct_prop\(X,Y,\s*([^,]+),\s*(\w+)\)'),
            'i_array': re.compile(r'i_array\(X,Y,\s*(\w+),\s*([^,]+),\s*(\d+),\s*(\w+)\)'),
            'i_vprop': re.compile(r'i_vprop\(X,Y,\s*(\w+),\s*([^,]+),\s*(\w+)\)'),
            'i_inlay': re.compile(r'i_inlay\s*\(X,Y,\s*(\w+),\s*([^,]+),\s*(\w+)\)')
        }
        
        self.method_patterns = {
            'i_method': re.compile(r'i_method\s*\(X,Y,\s*(\w+),\s*([^,]+),\s*(\w+)(?:,\s*([^)]+))?\)'),
            'i_guard': re.compile(r'i_guard\s*\(X,Y,\s*(\w+),\s*([^,]+),\s*(\w+)(?:,\s*([^)]+))?\)'),
            'i_override': re.compile(r'i_override\(X,Y,\s*(\w+),\s*(\w+)\)'),
            'i_ctr': re.compile(r'i_ctr\s*\(X,Y,\s*(\w+),\s*([^)]+)\)'),
            's_method': re.compile(r's_method\s*\(X,Y,\s*(\w+),\s*([^,]+),\s*(\w+)(?:,\s*([^)]+))?\)'),
            'i_cast': re.compile(r'i_cast\s*\(X,Y,\s*(\w+),\s*([^)]+)\)'),
            'i_attr': re.compile(r'i_attr\s*\(X,Y,\s*([^,]+),\s*(\w+),\s*(\d+),\s*([^)]+)\)'),
        }
        
        self.enum_patterns = {
            'enum_value': re.compile(r'enum_value\(E,T,Y,\s*(\w+),\s*([^)]+)\)'),
            'enum_value_v': re.compile(r'enum_value_v\(E,T,Y,\s*(\w+),\s*([^)]+)\)'),
        }

    def clean_line(self, line: str) -> str:
        """Clean and normalize a line"""
        return line.strip().rstrip('\\').strip()

    def parse_meta_attributes(self, meta_str: str) -> Dict[str, str]:
        """Parse meta attributes like 'meta, weak' or 'meta, f64'"""
        attrs = {}
        if not meta_str:
            return attrs
            
        parts = [p.strip() for p in meta_str.split(',')]
        for part in parts:
            if part == 'weak':
                attrs['weak'] = True
            elif part == 'meta':
                continue  # Skip meta keyword
            elif part == 'as':
                attrs['as'] = part.split(',')[1].strip() if ',' in part else part[2:].strip()
            else:
                attrs['type'] = part
        return attrs

    def convert_type(self, c_type: str) -> str:
        """Convert C types to Silver format types"""
        type_map = {
            'string': 'str',
            'cstr': 'str',
            'i64': 'i64',
            'i32': 'i32',
            'u32': 'u32',
            'u8': 'u8',
            'f32': 'f32',
            'f64': 'f64',
            'bool': 'bool',
            'none': 'none',
            'object': 'object',
            'handle': 'handle',
            'ARef': 'ARef',
            'num': 'num',
        }
        
        # Handle pointer types
        if c_type.endswith('*'):
            base_type = c_type[:-1].strip()
            return f"{type_map.get(base_type, base_type)}*"
            
        return type_map.get(c_type.strip(), c_type.strip())

    def parse_method_signature(self, method_line: str) -> Tuple[str, str, List[str], str]:
        """Parse method signature and return visibility, return_type, params, name"""
        # Handle different method patterns
        for pattern_name, pattern in self.method_patterns.items():
            match = pattern.search(method_line)
            if match:
                if pattern_name == 'i_method' or pattern_name == 'i_guard':
                    visibility = match.group(1)
                    return_type = match.group(2)
                    method_name = match.group(3)
                    params_str = match.group(4) if len(match.groups()) > 3 else ""
                    
                    # Parse parameters
                    params = []
                    if params_str:
                        param_parts = [p.strip() for p in params_str.split(',')]
                        for i in range(0, len(param_parts), 2):
                            if i + 1 < len(param_parts):
                                param_name = param_parts[i + 1]
                                param_type = param_parts[i]
                                params.append(f"{param_name}: {self.convert_type(param_type)}")
                    
                    return visibility, self.convert_type(return_type), params, method_name
                
                elif pattern_name == 'i_override':
                    return match.group(1), 'none', [], match.group(2)
                    
                elif pattern_name == 'i_ctr':
                    visibility = match.group(1)
                    param_type = match.group(2)
                    return visibility, 'none', [f"param: {self.convert_type(param_type)}"], 'constructor'
        
        return 'public', 'none', [], 'unknown'

    def extract_class_info(self, content: str) -> List[Dict]:
        """Extract all class definitions from the content"""
        classes = []
        lines = content.split('\n')
        i = 0
        
        while i < len(lines):
            line = self.clean_line(lines[i])
            
            # Look for schema definition
            schema_match = self.class_patterns['schema'].search(line)
            if schema_match:
                class_name = schema_match.group(1)
                
                # Find the schema content
                schema_content = []
                i += 1
                brace_count = 0
                
                while i < len(lines):
                    content_line = self.clean_line(lines[i])
                    if not content_line:
                        i += 1
                        continue
                        
                    # Look for declare_class statements
                    declare_match = None
                    inheritance = []
                    
                    for pattern_name, pattern in self.class_patterns.items():
                        if pattern_name.startswith('declare_'):
                            declare_match = pattern.search(content_line)
                            if declare_match:
                                if pattern_name == 'declare_class_2':
                                    inheritance = [declare_match.group(2)]
                                    if len(declare_match.groups()) > 2 and declare_match.group(3):
                                        inheritance.append(declare_match.group(3))
                                elif pattern_name == 'declare_class_3':
                                    inheritance = [declare_match.group(2), declare_match.group(3)]
                                break
                    
                    if declare_match:


                        declared_name = declare_match.group(1)
                        if declared_name != class_name:
                            print(f'{declared_name} != {class_name}')
                            # Skip this declaration—it doesn't match the schema name
                            i += 1
                            continue
                        else:
                            print(f'!! {declared_name} == {class_name}  !!')

                        # Process the schema content we've collected
                        class_info = self.parse_schema_content(class_name, schema_content, inheritance)
                        if class_info:
                            classes.append(class_info)
                        break
                    else:
                        schema_content.append(content_line)
                    
                    i += 1
            else:
                i += 1
        
        return classes

    def parse_schema_content(self, class_name: str, content_lines: List[str], inheritance: List[str]) -> Dict:
        """Parse the content of a schema definition"""
        properties = []
        methods = []
        
        for line in content_lines:
            line = line.strip()
            if not line or line.startswith('//') or line.startswith('#'):
                continue
            
            # Try to match property patterns
            prop_matched = False
            for pattern_name, pattern in self.property_patterns.items():
                match = pattern.search(line)
                if match:
                    prop_matched = True
                    if pattern_name == 'i_prop':
                        visibility = match.group(1)
                        prop_type = match.group(2)
                        prop_name = match.group(3)
                        meta = match.group(4) if len(match.groups()) > 3 else ""
                        
                        meta_attrs = self.parse_meta_attributes(meta)
                        
                        # format the property
                        prop_str = f"{visibility} {prop_name} : {self.convert_type(prop_type)}"
                        #if len(meta_attrs):
                            #prop_str += ','
                            #each m in items(meta_attrs):
                            #    prop_str += f' {m}'
                        
                        properties.append(prop_str)
                    
                    elif pattern_name == 'i_struct_prop':
                        prop_type = match.group(1)
                        prop_name = match.group(2)
                        properties.append(f"public {prop_name} : {self.convert_type(prop_type)}")
                    
                    elif pattern_name == 'i_array':
                        visibility = match.group(1)
                        array_type = match.group(2)
                        size = match.group(3)
                        prop_name = match.group(4)
                        properties.append(f"{visibility} {prop_name} : array[ {self.convert_type(array_type)} {size} ]")
                    
                    elif pattern_name == 'i_inlay':
                        visibility = match.group(1)
                        prop_type = match.group(2)
                        prop_name = match.group(3)
                        properties.append(f"{visibility} {prop_name} : {self.convert_type(prop_type)}")
                    
                    elif pattern_name == 'i_inlay':
                        visibility = match.group(1)
                        prop_type = match.group(2)
                        prop_name = match.group(3)
                        properties.append(f"{visibility} {prop_name} : {self.convert_type(prop_type)}")
                    
                    elif pattern_name == 'i_vprop':
                        # i_vprop(X,Y, public,  vertex_member_t, members) \
                        visibility = match.group(1)
                        prop_type = match.group(2)
                        prop_name = match.group(3)
                        properties.append(f"{visibility} {prop_name} : ref {self.convert_type(prop_type)}")

                    else:
                        raise RuntimeError(f'unhandled pattern: {pattern_name}')

                    break
            
            # Try to match method patterns if not a property
            if not prop_matched:
                for pattern_name, pattern in self.method_patterns.items():
                    match = pattern.search(line)
                    if match:
                        if pattern_name == 'i_method' or pattern_name == 'i_guard':
                            visibility = match.group(1)
                            return_type = match.group(2)
                            method_name = match.group(3)
                            params_str = match.group(4) if len(match.groups()) > 3 else ""
                            
                            # Parse parameters
                            params = []
                            if params_str:
                                param_parts = [p.strip() for p in params_str.split(',')]
                                for i in range(0, len(param_parts), 2):
                                    if i + 1 < len(param_parts):
                                        param_type = param_parts[i]
                                        param_name = param_parts[i + 1]
                                        params.append(f"{param_name}: {self.convert_type(param_type)}")
                            
                            param_str = "[ " + ", ".join(params) + " ]" if params else ""
                            if visibility == 'public':
                                methods.append(f"extern {method_name}{param_str} -> {self.convert_type(return_type)}")
                        
                        elif pattern_name == 'i_override':
                            scope = match.group(1)
                            method_name = match.group(2)
                            methods.append(f"override {method_name}")
                        
                        break
        
        return {
            'name': class_name,
            'inheritance': inheritance,
            'properties': properties,
            'methods': methods
        }

    def format_class(self, class_info: Dict) -> str:
        """Format a class definition in Silver format with aligned colons"""
        result = []

        # Class declaration
        class_line = f"class {class_info['name']}"
        if class_info['inheritance']:
            class_line += " [ " + ", ".join(class_info['inheritance']) + " ]"
        result.append(class_line)

        # Align properties
        props = class_info['properties']
        methods = class_info['methods']
        
        if props:
            # Split props into (name, rest) at first colon
            split_props = []
            max_name_len = 0
            max_arr_len = 0
            for p in props:
                if ":" in p:
                    col = ":" in p;
                    name, rest = p.split(":", 1)
                    name = name.rstrip()
                    max_name_len = max(max_name_len, len(name))
                    split_props.append((name, rest.strip(), ":"))
                elif "->" in p:
                    col = "->" in p;
                    name, rest = p.split("->", 1)
                    name = name.rstrip()
                    max_arr_len = max(max_arr_len, len(name))
                    split_props.append((name, rest.strip(), "->"))
                else:
                    split_props.append((p, ""))  # fallback
                    max_name_len = max(max_name_len, len(p))
                    max_arr_len = max(max_arr_len, len(p))

            for name, rest, op in split_props:
                if op == ":":
                    line = f"    {name.ljust(max_name_len)} : {rest}"
                elif op == "->":
                    line = f"    {name.ljust(max_arr_len)} -> {rest}"
                elif rest:
                    line = f"    {name} {rest}"
                else:
                    line = f"    {name}"
                result.append(line)

        # Add blank line between props and methods if both exist
        if props and methods:
            result.append("")

        # Methods (no colon alignment needed here)
        for method in methods:
            result.append(f"    {method}")

        return "\n".join(result)


    def convert(self, c_macro_content: str) -> str:
        """Convert C macro schema to Silver format"""
        classes = self.extract_class_info(c_macro_content)
        
        if not classes:
            return "# No classes found in the input"
        
        result = []
        for class_info in classes:
            result.append(self.format_class(class_info))
            result.append("")  # Add blank line between classes
        
        return "\n".join(result).rstrip()

def concatenate_headers(lib_dir: str, project_name: str) -> str:
    """Concatenate headers starting with project header, then .h files"""
    import os
    import glob
    
    content_parts = []
    
    # Start with main project header (no extension)
    main_header = os.path.join(lib_dir, project_name)
    if os.path.exists(main_header):
        try:
            with open(main_header, 'r', encoding='utf-8') as f:
                content_parts.append(f"// === {project_name} (main header) ===\n")
                content_parts.append(f.read())
                content_parts.append("\n")
        except Exception as e:
            print(f"Warning: Could not read main header {main_header}: {e}")
    else:
        print(f"Warning: Main header {main_header} not found")
    
    # Then add all .h files
    h_files = glob.glob(os.path.join(lib_dir, "*.h"))
    h_files.sort()  # Process in consistent order
    
    for h_file in h_files:
        filename = os.path.basename(h_file)
        try:
            with open(h_file, 'r', encoding='utf-8') as f:
                content_parts.append(f"// === {filename} ===\n")
                content_parts.append(f.read())
                content_parts.append("\n")
        except Exception as e:
            print(f"Warning: Could not read header {h_file}: {e}")
    
    return "".join(content_parts)

def main():
    if len(sys.argv) == 4 and sys.argv[1] == "--concat":
        # Concatenation mode: python converter.py --concat <lib_dir> <project_name>
        lib_dir = sys.argv[2]
        project_name = sys.argv[3]
        
        print(f"Concatenating headers from {lib_dir} starting with {project_name}")
        concatenated = concatenate_headers(lib_dir, project_name)
        
        # Convert the concatenated content
        converter = SilverConverter()
        result = converter.convert(concatenated)
        
        # Output to stdout (can be redirected)
        print(result)
        
    elif len(sys.argv) == 5 and sys.argv[1] == "--concat":
        # Concatenation with output file: python converter.py --concat <lib_dir> <project_name> <output_file>
        lib_dir = sys.argv[2]
        project_name = sys.argv[3]
        output_file = sys.argv[4]
        
        print(f"Concatenating headers from {lib_dir} starting with {project_name}")
        concatenated = concatenate_headers(lib_dir, project_name)
        
        # Convert the concatenated content
        converter = SilverConverter()
        result = converter.convert(concatenated)
        
        # Write to output file
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(result)
        print(f"Conversion complete! Output written to {output_file}")
        
    elif len(sys.argv) == 3:
        # Original mode: python converter.py <input_file> <output_file>
        input_file = sys.argv[1]
        output_file = sys.argv[2]
        
        # Read input
        if input_file == "-":
            content = sys.stdin.read()
        else:
            with open(input_file, 'r', encoding='utf-8') as f:
                content = f.read()
        
        # Convert
        converter = SilverConverter()
        result = converter.convert(content)
        
        # Write output
        if output_file == "-":
            print(result)
        else:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(result)
            print(f"Conversion complete! Output written to {output_file}")
    else:
        print("C Macro to Silver Format Converter")
        print("\nUsage:")
        print("  # Convert single file:")
        print("  python converter.py <input_file> <output_file>")
        print("  python converter.py - - (to use stdin/stdout)")
        print()
        print("  # Concatenate headers and convert:")
        print("  python converter.py --concat <lib_dir> <project_name> [output_file]")
        print("  python converter.py --concat ./lib trinity > trinity.sf")
        print("  python converter.py --concat ./lib myproject myproject.sf")
        sys.exit(1)

if __name__ == "__main__":
    # Example usage if run directly
    if len(sys.argv) == 1:
        print("C Macro to Silver Format Converter")
        print("\nUsage:")
        print("  # Convert single file:")
        print("  python converter.py input.h output.sf")
        print("  cat input.h | python converter.py - - > output.sf")
        print()
        print("  # Concatenate headers and convert:")
        print("  python converter.py --concat ./lib trinity > trinity.sf")
        print("  python converter.py --concat ./lib myproject myproject.sf")
        print("\nFor interactive testing, provide input content:")
        
        # Simple test
        test_input = """
#define trinity_schema(X,Y,...) \\
    i_prop    (X,Y, intern,     VkInstance,          instance,        as, ARef) \\
    i_prop    (X,Y, intern,     i64,                 msaa_samples) \\
    i_prop    (X,Y, public,     i32,                 queue_family_index) \\
    i_method  (X,Y, public,     texture,             environment, image, vec3f, f32) \\
    i_override(X,Y, method,     init) \\
    i_override(X,Y, method,     dealloc)
declare_class(trinity)
"""
        
        converter = SilverConverter()
        result = converter.convert(test_input)
        print("\nExample conversion:")
        print("Input (C Macro):")
        print(test_input)
        print("\nOutput (Silver Format):")
        print(result)
    else:
        main()