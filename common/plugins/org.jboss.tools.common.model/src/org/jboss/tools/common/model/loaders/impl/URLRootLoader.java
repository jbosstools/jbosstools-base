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

import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

import org.w3c.dom.Element;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XMLUtil;
import org.jboss.tools.common.util.FileUtil;

public class URLRootLoader extends FileRootLoader {

    public URLRootLoader() {}

    public void load(XModelObject object) {
        if(isFilePath(getPath(object))) {
            super.load(object);
        } else {
            try {
                Element element = XMLUtil.getElement(getInputStream(object));
                if(element != null) util().load(element, object);
            } catch (Exception e) {
            	ModelPlugin.getPluginLog().logError(e);
            }
        }
    }

    public boolean save(XModelObject object) {
        if(isFilePath(getPath(object))) {
            return super.save(object);
        } else {
            return true;
        }
    }

    public String getPath(XModelObject object) {
        return FileUtil.fileURLToFilePath(super.getPath(object));
    }

    public boolean isFilePath(String path) {
        return (path != null && path.lastIndexOf(':') < 2);
    }

    public InputStream getInputStream(XModelObject object) {
        try {
            return new URL(getPath(object)).openConnection().getInputStream();
        } catch (Exception e) {
        	//ignore
            return null;
        }
    }

    public OutputStream getOutputStream(XModelObject object) {
        try {
            return new URL(getPath(object)).openConnection().getOutputStream();
        } catch (Exception e) {
        	//ignore
            return null;
        }
    }

}
