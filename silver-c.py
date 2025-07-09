#!/usr/bin/env python3
"""
Silver to C Macro Format Converter

Converts Silver class definitions to C macro schema format.
"""

import re
import sys
from typing import List, Dict, Tuple, Optional
from dataclasses import dataclass


@dataclass
class Property:
    """Represents a class property"""
    visibility: str  # public, intern, required
    type_name: str
    name: str
    params: List[Tuple[str, str]] = None  # For methods: [(param_name, param_type), ...]
    return_type: str = None  # For methods
    is_weak: bool = False
    meta_type: str = None
    array_size: Optional[int] = None
    cast_as: str = None  # For "as" conversions


@dataclass
class ClassDef:
    """Represents a complete class definition"""
    name: str
    base_classes: List[str]
    properties: List[Property]
    methods: List[Property]
    overrides: List[str]
    arrays: List[Property]
    enums: Dict[str, List[Tuple[str, str]]]  # enum_name -> [(value, number), ...]
    is_struct: bool = False


class SilverToCMacroConverter:
    def __init__(self):
        self.classes: Dict[str, ClassDef] = {}
        self.current_class = None
        self.enum_definitions: Dict[str, List[Tuple[str, str]]] = {}
        
    def parse_silver(self, silver_content: str) -> None:
        """Parse Silver format content"""
        # Remove C++ style comments
        silver_content = re.sub(r'//.*$', '', silver_content, flags=re.MULTILINE)
        # Remove C style comments
        silver_content = re.sub(r'/\*.*?\*/', '', silver_content, flags=re.DOTALL)
        
        lines = silver_content.strip().split('\n')
        
        for line in lines:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
                
            if line.startswith('class '):
                self._parse_class_definition(line)
            elif self.current_class and ':' in line:
                self._parse_class_member(line)
                
    def _parse_class_definition(self, line: str) -> None:
        """Parse class definition line"""
        # Match patterns like "class trinity:" or "class mouse_state:"
        match = re.match(r'class\s+(\w+)\s*(?::\s*(.+?))?\s*:?\s*$', line)
        if match:
            class_name = match.group(1)
            base_classes = []
            
            if match.group(2):
                # Parse base classes if present
                base_classes = [b.strip() for b in match.group(2).split(',')]
            
            self.current_class = ClassDef(
                name=class_name,
                base_classes=base_classes,
                properties=[],
                methods=[],
                overrides=[],
                arrays=[],
                enums={},
                is_struct=class_name in ['mouse_state', 'keyboard_state', 'xcoord', 'ycoord', 'text_metrics']
            )
            self.classes[class_name] = self.current_class
    
    def _parse_class_member(self, line: str) -> None:
        """Parse a class member (property or method)"""
        if not self.current_class:
            return
            
        # Remove comments
        line = line.split('#')[0].strip()
        if not line:
            return
            
        # Handle override declarations
        if line.startswith('override '):
            override_name = line.replace('override', '').strip()
            self.current_class.overrides.append(override_name)
            return
            
        # Parse visibility and member
        parts = line.split(None, 1)
        if len(parts) < 2:
            return
            
        visibility = parts[0]
        rest = parts[1]
        
        # Check if it's a method (has parameters)
        if '[' in rest and ']' in rest:
            self._parse_method(visibility, rest)
        else:
            self._parse_property(visibility, rest)
    
    def _parse_property(self, visibility: str, declaration: str) -> None:
        """Parse a property declaration"""
        # Handle array properties
        array_match = re.match(r'(\w+)\s*\[\s*(\w+)\s*,\s*(\d+)\s*\]\s*:\s*(\w+)', declaration)
        if array_match:
            name = array_match.group(1)
            type_name = array_match.group(2)
            size = int(array_match.group(3))
            # The fourth group might be the element type
            prop = Property(
                visibility=visibility,
                type_name=type_name,
                name=name,
                array_size=size
            )
            self.current_class.arrays.append(prop)
            return
            
        # Regular property
        parts = declaration.split(':', 1)
        if len(parts) != 2:
            return
            
        name = parts[0].strip()
        type_info = parts[1].strip()
        
        # Parse type with potential modifiers
        type_parts = type_info.split(',')
        type_name = type_parts[0].strip()
        
        prop = Property(
            visibility=visibility,
            type_name=type_name,
            name=name
        )