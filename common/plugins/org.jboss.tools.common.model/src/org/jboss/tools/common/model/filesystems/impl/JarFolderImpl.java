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
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.filesystems.*;

public class JarFolderImpl extends RegularObjectImpl implements FolderLoader {
    private static final long serialVersionUID = 7958999905551184060L;
    protected boolean loaded = false;

    public JarFolderImpl() {}

    public int getFileType() {
        return FOLDER;
    }
    
    protected Comparator<XModelObject> createComparator() {
        return new FileObjectComparator();
    }

    protected JarSystemImpl getJarSystem() {
        JarFolderImpl folder = (JarFolderImpl)getParent();
        return (folder == null) ? null : folder.getJarSystem();
    }

    public boolean isObjectEditable() {
        return false;
    }

    protected String getAbsolutePath() {
        String p = (getParent() == null) ? null : ((JarFolderImpl)getParent()).getAbsolutePath();
        if(p != null && p.length() > 0) p += XModelObjectConstants.SEPARATOR;
        return (p == null) ? null : p + name();
    }

    public BodySource getBodySource(String filename) {
        String path = getAbsolutePath();
        if(path == null) return null;
        String cpath = (path.length() == 0) ? filename : path + XModelObjectConstants.SEPARATOR + filename;
        return new JarBodySource(getJarSystem().getJarAccess(), cpath);
    }

    protected void loadChildren() {
        if(loaded || !isActive()) return;
        JarAccess jar = getJarSystem().getJarAccess();
        if(!jar.isLoaded()) return;
        jar.lockJar();
        loaded = true;
        String path = getAbsolutePath();
        String[] cs = jar.getChildren(path);
        Properties p = new Properties();
        for (int i = 0; i < cs.length; i++) {
            boolean d = cs[i].endsWith(XModelObjectConstants.SEPARATOR);
            if(d) cs[i] = cs[i].substring(0, cs[i].length() - 1);
            if(d) {
                p.clear();
                p.setProperty(XModelObjectConstants.ATTR_NAME, cs[i]);
                XModelObject c = getModel().createModelObject("JarFolder", p); //$NON-NLS-1$
                addChild(c);
            } else {
                createFileObject(jar, path, cs[i]);
            }
        }
        fire = true;
        jar.unlockJar();
    }

    private void createFileObject(JarAccess jar, String path, String name) {
        String cpath = (path.length() == 0) ? name : path + XModelObjectConstants.SEPARATOR + name;
        Properties p = new Properties();
        FolderImpl.parseFileName(p, name);
        String ext = p.getProperty(XModelObjectConstants.ATTR_NAME_EXTENSION);
        String body = null;
        String entity = getModel().getEntityRecognizer().getEntityName(ext, body);
        if("FileAny".equals(entity)) { //$NON-NLS-1$
            if(jar.getSize(cpath) > 100000) entity = XModelObjectConstants.ENT_FILE_ANY_LONG;
            else if(jar.isTextEntry(cpath, 100)) entity = "FileTXT"; //$NON-NLS-1$
        } else /*if(entity == null)*/ {
            body = jar.getContent(cpath);
            entity = getModel().getEntityRecognizer().getEntityName(ext, body);
        }
        if(entity == null || getModel().getMetaData().getEntity(entity) == null) entity = "FileAny"; //$NON-NLS-1$
        XModelObject c = getModel().createModelObject(entity, p);
        if(FolderImpl.isLateloadFile2(c)) {
            FileAnyImpl ci = (FileAnyImpl)c;
            ci.setBodySource(new JarBodySource(jar, cpath));
        } else {
            XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(c);
            if(loader != null) {
                if(body == null) body = jar.getContent(cpath);
                XModelObjectLoaderUtil.setTempBody(c, body);
                loader.load(c);
            }
        }
        addChild(c);
    }

    protected boolean fire = false;

    protected void fireStructureChanged(int kind, Object info) {
        if(fire) super.fireStructureChanged(kind, info);
    }

    public boolean hasChildren() {
        boolean q = super.hasChildren();
        if (q || loaded) return q;
        loadChildren();
        return super.hasChildren();
    }

    public String getPathPart() {
        String s = super.getPathPart();
        return FilePathHelper.toPathPath(s);
    }

    public XModelObject getChildByPathPart(String pathpart) {
    	pathpart = FilePathHelper.toPathPath(pathpart);
        return super.getChildByPathPart(pathpart);
    }

    public boolean update() {
        return true;
    }
    
    public boolean save() {
        return true;
    }

	public boolean isRemoved() {
		return false;
	}

}

class JarBodySource implements BodySource {
    private JarAccess jar = null;
    private String path = null;

    public JarBodySource(JarAccess jar, String path) {
        this.jar = jar;
        this.path = path;
    }

    public String get() {
        return jar.getContent(path);
    }

    public boolean write(Object object) {
        return true;
    }

}

