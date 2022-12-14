/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package org.gradle.transform.javamodules;

import org.gradle.internal.impldep.org.eclipse.jgit.annotations.NonNull;
import org.gradle.internal.impldep.org.eclipse.jgit.annotations.Nullable;

import java.io.Serial;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Iterator;

public class Tuple2<T1, T2> implements Iterable<Object>, Serializable {
    @Serial
    private static final long serialVersionUID = -3518082018884860684L;
    @NonNull
    final T1 t1;
    @Nullable
    final T2 t2;

    Tuple2(T1 t1, T2 t2) {
        this.t1 = Objects.requireNonNull(t1, "t1");
        this.t2 = t2;
    }

    static <T1, T2> Tuple2<T1, T2> of(T1 t1, T2 t2) {
        return new Tuple2<>(t1, t2);
    }

    public T1 getT1() {
        return this.t1;
    }

    public T2 getT2() {
        return this.t2;
    }

    public List<Object> toList() {
        return Arrays.asList(this.toArray());
    }

    public Object[] toArray() {
        return new Object[]{this.t1, this.t2};
    }

    public Iterator<Object> iterator() {
        return Collections.unmodifiableList(this.toList()).iterator();
    }

}
