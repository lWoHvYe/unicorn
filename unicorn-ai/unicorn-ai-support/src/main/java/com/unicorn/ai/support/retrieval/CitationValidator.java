/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

package com.unicorn.ai.support.retrieval;

import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

@Component
public class CitationValidator {

    private static final Pattern CITATION = Pattern.compile("\\[C(\\d+)]");

    public boolean valid(String answer, List<Citation> citations) {
        return invalidReferences(answer, citations).isEmpty();
    }

    public List<Integer> invalidReferences(String answer, List<Citation> citations) {
        var invalid = new ArrayList<Integer>();
        if (answer == null || answer.isBlank()) {
            return invalid;
        }
        var matcher = CITATION.matcher(answer);
        while (matcher.find()) {
            try {
                var index = Integer.parseInt(matcher.group(1));
                if (index < 1 || index > citations.size()) {
                    invalid.add(index);
                }
            } catch (NumberFormatException exception) {
                invalid.add(-1);
            }
        }
        return invalid;
    }

    public String refusalFor(List<Integer> invalid) {
        return "回答引用校验失败（引用不存在：" + invalid + "），请人工复核或补充知识库资料。";
    }
}
