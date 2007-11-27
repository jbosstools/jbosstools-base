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
package org.jboss.tools.common.meta.constraint.impl;

import java.util.*;

public class XAttributeConstraintListAdd extends XAttributeConstraintAList {

    public XAttributeConstraintListAdd() {}

    public boolean accepts(String value) {
        StringTokenizer st = new StringTokenizer(value, ",;");
        while(st.hasMoreTokens()) {
            String t = st.nextToken().trim();
            if(!values.contains(t)) return false;
        }
        return true;
    }

}

