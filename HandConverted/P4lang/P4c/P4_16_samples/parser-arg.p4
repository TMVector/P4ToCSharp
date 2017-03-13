/*
Copyright 2016 VMware, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <core.p4>

parser Parser();
package Package(Parser p1, Parser p2);

parser Parser1()(Parser p) {
    state start {
        p.apply();
        transition accept;
    }
}

parser Parser2()(Parser p) {
    state start {
        p.apply();
        transition accept;
    }
}

parser Inside() {
    state start { transition accept; }
}

Parser1(Inside()) p1;
Parser2(Inside()) p2;

Package(p1,p2) main;
