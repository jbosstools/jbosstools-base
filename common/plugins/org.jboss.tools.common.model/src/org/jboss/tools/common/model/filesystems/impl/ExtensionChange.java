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

import java.io.File;
import java.util.Properties;

import org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultRemoveHandler;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.loaders.EntityRecognizerContext;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.undo.XTransactionUndo;
import org.jboss.tools.common.model.undo.XUndoManager;
import org.jboss.tools.common.model.util.FindObjectHelper;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.jboss.tools.common.util.FileUtil;

public class ExtensionChange {
    private XModelObject file = null;
    private String extension = null;

    public ExtensionChange() {}

    public boolean execute(XModelObject file, String extension) {
        this.file = file;
        this.extension = extension;
        return execute();
    }

    private boolean execute() {
        XModel model = file.getModel();
        String entity = model.getEntityRecognizer().getEntityName(new EntityRecognizerContext(extension));
        String oldEntity = file.getModelEntity().getName();
        String body = __body();
        if(body == null) return false;
        if("FileAny".equals(entity)) {
        	if(XModelObjectConstants.ENT_FILE_ANY_LONG.equals(oldEntity)) return false;
            if(FileUtil.isText(body)) entity = "FileTXT";
            else return false;
        } else if(entity == null) {
            entity = model.getEntityRecognizer().getEntityName(new EntityRecognizerContext(extension, body));
        }
        if(entity == null || model.getMetaData().getEntity(entity) == null) entity = "FileAny";
        if(file.getModelEntity().getName().equals(entity)) return false;
        XModelObject o = createFileObject(entity, body);
        if(o == null) return false;
        if(o.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME__FILE) != null &&
        	file.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME__FILE) != null) {
        	o.set(XModelObjectConstants.ATTR_NAME__FILE, file.get(XModelObjectConstants.ATTR_NAME__FILE));
        }
        XUndoManager undo = model.getUndoManager();
        String d = "Change extension " + file.getParent().getAttributeValue(XModelObjectConstants.ATTR_NAME);
        XTransactionUndo u = new XTransactionUndo(d, XTransactionUndo.EDIT);
        undo.addUndoable(u);
        try {
            XModelObject p = file.getParent();
            DefaultRemoveHandler.removeFromParent(file);
            DefaultCreateHandler.addCreatedObject(p, o, FindObjectHelper.IN_NAVIGATOR_ONLY);
        } catch (XModelException e) {
            undo.rollbackTransactionInProgress();
            return false;
        } finally {
            u.commit();
        }
        return true;
    }

    private String __body() {
        XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(file);
        if(loader != null) {
            file.setModified(true);
            loader.save(file);
            return XModelObjectLoaderUtil.getTempBody(file);
        } else if(file.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME__FILE) != null) {
            String sfn = file.get(XModelObjectConstants.ATTR_NAME__FILE);
            if(sfn.length() == 0) return null;
            File sf = new File(sfn);
            return (sf.isFile()) ? FileUtil.readFile(sf) : null;
        } else {
            return file.getAttributeValue(XModelObjectConstants.ATTR_NAME_BODY);
        }
    }

    private XModelObject createFileObject(String entity, String body) {
        Properties p = new Properties();
        p.setProperty(XModelObjectConstants.ATTR_NAME, file.getAttributeValue(XModelObjectConstants.ATTR_NAME));
        p.setProperty(XModelObjectConstants.ATTR_NAME_EXTENSION, extension);
        XModelObject c = file.getModel().createModelObject(entity, p);
        XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(c);
        if(loader != null) {
            XModelObjectLoaderUtil.setTempBody(c, body);
            loader.load(c);
        } else {
            c.setAttributeValue(XModelObjectConstants.ATTR_NAME_BODY, body);
        }
        return c;
    } 

}
