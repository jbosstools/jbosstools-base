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
package org.jboss.tools.common.model.loaders.impl;

import java.io.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.util.*;

public class FileRootLoader implements XObjectLoader {
    protected static XModelObjectLoaderUtil util = new XModelObjectLoaderUtil();

    public FileRootLoader() {}

    protected XModelObjectLoaderUtil util() {
        return util;
    }

    public void load(XModelObject object) {
        util().load(file(object), object);
    }

    public boolean update(XModelObject object) {
        return true;
    }

    public boolean save(XModelObject object) {
        return util().save(file(object), object);
    }

    public File file(XModelObject object) {
        return new File(getPath(object));
    }

    public String getPath(XModelObject object) {
        return fileroot(object) + fileName(object);
    }

    public String fileroot(XModelObject object) {
        return XModelConstants.getHome(object.getModel()) + "/";
    }

    protected String fileName(XModelObject object) {
        return object.getModelEntity().getName().toLowerCase() + ".pex";
    }

}

