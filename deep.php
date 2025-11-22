<?php

function flatten(array $a) {
    $flattened = [];

    while (true) {
        if (empty($a))
            return $flattened;

        $head = &$a[0];
        
        if (is_array($head)) {
            if (empty($head)) {
                array_shift($a);
            } else {
                array_unshift($a, array_shift($head));
            }
        } else {
            $flattened[] = array_shift($a);
        }
    }
}

var_dump(flatten([1, [2, 3, [], [4, 5, 6], 7], 8, [9]]));
