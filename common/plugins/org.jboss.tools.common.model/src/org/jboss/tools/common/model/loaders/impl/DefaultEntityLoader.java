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
import org.w3c.dom.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class DefaultEntityLoader implements XObjectLoader {
    protected static XModelObjectLoaderUtil util = new XModelObjectLoaderUtil();

    public DefaultEntityLoader() {}

    public void load(XModelObject object) {
        String body = XModelObjectLoaderUtil.getTempBody(object);
        Element e = XMLUtil.getElement(new StringReader(body));
        if(e != null) util().load(e, object);
    }

    public boolean update(XModelObject object) {
        XModelObject c = object.copy(0);
        XModelObjectLoaderUtil.setTempBody(c, XModelObjectLoaderUtil.getTempBody(object));
        load(c);
        XModelObject p = object.getParent();
        p.removeChild(object);
        p.addChild(c);
        c.setModified(false);
        return true;
    }

    public boolean save(XModelObject object) {
        if(!object.isModified()) return true;
        try {
            StringWriter sw = new StringWriter();
            if(!util().serialize(object, sw)) return false;
            XModelObjectLoaderUtil.setTempBody(object, sw.toString());
            object.setModified(true);
            return true;
        } catch (Exception exc) {
        	ModelPlugin.getPluginLog().logError("DefaultEntityLoader:save(" + object.getPresentationString() + "):" + exc.getMessage());
            return false;
        }
    }

    protected XModelObjectLoaderUtil util() {
        return util;
    }

}

