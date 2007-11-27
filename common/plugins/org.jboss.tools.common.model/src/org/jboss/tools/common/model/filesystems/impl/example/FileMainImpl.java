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
package org.jboss.tools.common.model.filesystems.impl.example;

import org.jboss.tools.common.model.filesystems.*;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.util.*;

public class FileMainImpl extends RecognizedFileImpl {
    private static final long serialVersionUID = 2309879255283859914L;

    public boolean hasChildren() {
        return true;
    }

    public String get(String name) {
        if(!("NAME".equals(name) || "EXTENSION".equals(name))
           && getParent() != null) loadChildren();
        return super.get(name);
    }

    protected void loadChildren() {
        BodySource bs = getBodySource();
        if(bs == null) return;
        super.setBodySource(null);
        XModelObjectLoaderUtil.setTempBody(this, bs.get());
        XModelObjectLoaderUtil.getObjectLoader(this).load(this);
    }

}

