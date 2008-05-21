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
package org.jboss.tools.common.model.impl;

import java.util.*;
import org.jboss.tools.common.model.XModelObject;

public class PresentationComparator implements Comparator {

    public PresentationComparator() {}

    public int compare(Object o1, Object o2) {
        XModelObject s1 = (XModelObject)o1;
        XModelObject s2 = (XModelObject)o2;
        String p1 = s1.getPresentationString();
        String p2 = s2.getPresentationString();
        return String.CASE_INSENSITIVE_ORDER.compare(p1, p2);
    }

    public boolean equals(Object obj) {
        return obj == this;
    }

}
