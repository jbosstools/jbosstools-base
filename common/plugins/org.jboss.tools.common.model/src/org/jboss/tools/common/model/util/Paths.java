/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.util;

import java.util.*;

public class Paths {

    public static String expand(String str, Properties properties) {
        String result = str;
        if (str == null || str.length() == 0) {
            return result;
        }
        int p1 = -1;
        int p2 = -1;
        int iStartPos = 0;
        do {
            p1 = result.indexOf('%', iStartPos);
            p2 = (p1 >= 0) ? result.indexOf('%', p1 + 1) : -1;
            if (p1 >= 0 && p2 > (p1 + 1)) {
                String s = properties.getProperty(result.substring(p1 + 1, p2), "");
                result = result.substring(0, p1) + s + result.substring(p2 + 1);
                iStartPos = p1;
            } else {
                iStartPos = p1 + 2;
            }
        } while (p1 >= 0 && p2 > p1);
        return  result;
    }

}

