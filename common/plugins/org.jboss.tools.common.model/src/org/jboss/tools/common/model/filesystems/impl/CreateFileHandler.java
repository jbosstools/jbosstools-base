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

import org.eclipse.swt.widgets.Display;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.handlers.*;
import org.jboss.tools.common.meta.impl.XMetaDataConstants;
import org.jboss.tools.common.model.loaders.EntityRecognizerContext;
import org.jboss.tools.common.model.util.*;

public class CreateFileHandler extends DefaultCreateHandler {

    public void executeHandler(XModelObject object, Properties prop) throws XModelException {
        if(!isEnabled(object) || data == null || data.length == 0) return;
        Properties p = extractProperties(data[0]);
        String defaultExtention = getDefaultExtension(prop);
        validateNameAndExtension(action, p, defaultExtention);
        XModelObject parent = getParentFolder(object, p);
        String ext = p.getProperty(XModelObjectConstants.ATTR_NAME_EXTENSION);
        String entity = action.getProperty(XMetaDataConstants.ENTITY);
        if(entity == null) {
            entity = (ext != null) ? object.getModel().getEntityRecognizer().getEntityName(new EntityRecognizerContext(ext))
                        : data[0].getModelEntity().getName();
            if(entity == null || object.getModel().getMetaData().getEntity(entity) == null)
              entity = data[0].getModelEntity().getName();
        }
        setOtherProperties(object, p);
        XModelObject c = XModelObjectLoaderUtil.createValidObject(object.getModel(), entity, p);
        c = modifyCreatedObject(c);
        if(parent.isActive()) {
            addCreatedObject(parent, c, p);
        } else {
            addCreatedObject(parent, c, false, p);
            XModelObject po = (XModelObject)p.get("parentObject"); //$NON-NLS-1$
            XModelObject co = (XModelObject)p.get("childObject"); //$NON-NLS-1$
            addCreatedObject(po, co, p);
        }
        if(parent instanceof FolderImpl) {
            FolderImpl f = (FolderImpl)parent;
            f.saveChild(c);            	
        }
        final XModelObject q = c;
        if(q != null) Display.getDefault().asyncExec(new Runnable() {
        	public void run() {        
				XActionInvoker.invoke("Open", q, new Properties()); //$NON-NLS-1$
        	}
        });
    }

    private String getDefaultExtension(Properties prop) {
        String ext = (prop == null) ? null : prop.getProperty("defaultExtention"); //$NON-NLS-1$
        return (ext == null) ? null : ext;
    }

    public static void validateNameAndExtension(XAction action, Properties p, String defaultExtention) {
        String name = p.getProperty(XModelObjectConstants.ATTR_NAME);
        String ext = defaultExtention;
        String path = null;
        int i = name.lastIndexOf('.');
        if(i > 0) {
            String es = action.getProperty("extensions"); //$NON-NLS-1$
            String xext = name.substring(i + 1);
            if(es == null || es.length() == 0 || es.indexOf("." + xext + ".") >= 0) { //$NON-NLS-1$ //$NON-NLS-2$
                ext = xext;
                name = name.substring(0, i);
            }
        }
        name = name.replace('\\', '/');
        i = name.lastIndexOf('/');
        if(i >= 0) {
            path = name.substring(0, i);
            name = name.substring(i + 1);
        }
        p.setProperty(XModelObjectConstants.ATTR_NAME, name);
        if(ext != null) p.setProperty(XModelObjectConstants.ATTR_NAME_EXTENSION, ext);
        if(path != null) p.setProperty("path", path); //$NON-NLS-1$
    }

    private XModelObject getParentFolder(XModelObject object, Properties p) throws XModelException {
        String path = p.getProperty("path"); //$NON-NLS-1$
        if(path == null || path.length() == 0) return object;
        StringTokenizer st = new StringTokenizer(path, XModelObjectConstants.SEPARATOR);
        while(st.hasMoreTokens()) {
            String pp = st.nextToken();
            XModelObject c = object.getChildByPath(pp);
            if(c == null) {
                c = object.getModel().createModelObject("FileFolder", null); //$NON-NLS-1$
                c.setAttributeValue(XModelObjectConstants.ATTR_NAME, pp);
                p.put("parentObject", object); //$NON-NLS-1$
                p.put("childObject", c); //$NON-NLS-1$
                return createFolder(c, st);
            } else if(!"FileFolder".equals(c.getModelEntity().getName())) { //$NON-NLS-1$
                throw new XModelException("Cannot create folder " + pp + " in " + object.getPathPart());
            } else {
                object = c;
            }
        }
        return object;
    }

    private XModelObject createFolder(XModelObject object, StringTokenizer path) throws XModelException {
        while(path.hasMoreTokens()) {
            String pp = path.nextToken();
            XModelObject c = object.getModel().createModelObject("FileFolder", null); //$NON-NLS-1$
            c.setAttributeValue(XModelObjectConstants.ATTR_NAME, pp);
            object.addChild(c);
            object = c;
        }
        return object;
    }

}

