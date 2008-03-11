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

import org.jboss.tools.common.model.XModelObject;

public class XModelObjectCache {
    private XModelObject object;
    private String path;
    public XModelObjectCache(XModelObject o) {
        object = o;
        validate();
    }

    public XModelObject getObject() {
        validate();
        return object;
    }

    private void validate() {
        String p = object.getPath();
        if(p != null) {
            path = p;
        } else if(path != null) {
            XModelObject o = object.getModel().getByPath(path);
            if(o != null) object = o;
        }
    }

}

