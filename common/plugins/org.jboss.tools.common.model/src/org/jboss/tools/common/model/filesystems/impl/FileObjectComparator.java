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
package org.jboss.tools.common.model.filesystems.impl;

import java.util.*;
import org.jboss.tools.common.model.XModelObject;

public class FileObjectComparator implements Comparator<XModelObject> {

    public FileObjectComparator() {}

    public int compare(XModelObject o1, XModelObject o2) {
        int i1 = o1.getFileType();
        int i2 = o2.getFileType();
        if(i1 != i2) return (i2 - i1);
        return o1.getPathPart().toLowerCase().compareTo(o2.getPathPart().toLowerCase());
    }

    public boolean equals(Object obj) {
        return obj == this;
    }

}
