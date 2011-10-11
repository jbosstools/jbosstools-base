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

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;

public class JarSystemImpl extends JarFolderImpl implements org.jboss.tools.common.model.filesystems.FileSystem {
    private static final long serialVersionUID = 7958999759019059243L;
    protected JarAccess jar = null;

    public JarSystemImpl() {}

    public int getFileType() {
        return SYSTEM;
    }
    
    protected JarSystemImpl getJarSystem() {
        return this;
    }

    protected JarAccess getJarAccess() {
    	if(jar == null) {
    		jar = JarAccessFactory.getJarAccess(getLocation(), this);
    	}
        return jar;
    }

    public boolean isAttributeEditable(String name) {
        return super.isAttributeEditable(name) && XModelObjectConstants.ATTR_NAME.equals(name);
    }

    protected String getAbsolutePath() {
        return ""; //$NON-NLS-1$
    }
    
    boolean loaded2 = false;

    protected void loadChildren() {
    	if(!isActive()) {
    		return;
    	}
//        if(jar.isLoaded()) return;

        if(this != getJarAccess().getMain()) return;
        if(loaded2) return;

		synchronized (this) {
			jar.setLocation(getLocation());
			super.loadChildren();
		}
        loaded2 = true;
    }

    public XModelObject[] getChildren() {
    	JarSystemImpl main = getJarAccess().getMain();
    	return (main == this || main == null) ? super.getChildren() : main.getChildren();
    }

    public boolean hasChildren() {
    	JarSystemImpl main = getJarAccess().getMain();
    	return (main == this || main == null) ? super.hasChildren() : main.hasChildren();
    }

    public XModelObject getChildByPathPart(String pathpart) {
    	JarSystemImpl main = getJarAccess().getMain();
    	return (main == this || main == null) ? super.getChildByPathPart(pathpart) : main.getChildByPathPart(pathpart);
    }

    public String getPathPart() {
        return name();
    }
    
    public String getLocation() {
		return Paths.expand(get(XModelObjectConstants.ATTR_NAME_LOCATION), getModel().getProperties());
    }

    public String getTempLocation() {
    	JarSystemImpl main = getJarAccess().getMain();
    	if(main != this && main != null) {
    		main.getChildren();
    	} else if(!jar.isLoaded()) {
    		loadChildren();
    	}
        String s = jar.getTempLocation();
        return (s == null) ? get(XModelObjectConstants.ATTR_NAME_LOCATION) : s;
    }

    public LFileObject getFileObject(String relpath) {
        return getJarAccess().getFileObject(name(), relpath);
    }

    public boolean update() {
    	if(getJarAccess().getMain() != this) return true;

        if(jar.isModified()) {
            if(jar.isLoaded()) {
                XModelObject[] cs = getChildren();
                for (int i = 0; i < cs.length; i++) removeChild_0(cs[i]);
                jar.invalidate();
            }
            jarUpdated();

            JarSystemImpl[] ss = getJarAccess().getSlaves();
            for (JarSystemImpl s: ss) s.jarUpdated();
        }
        return true;
    }

    public void jarUpdated() {
        loaded = false;
        loaded2 = false;
        fire = true;
        fireStructureChanged(3, null);
    }

    public String getPresentationString() {
    	String location = getLocation();
    	if(location != null) {
    		location = location.replace('\\', '/');
    		int i = location.lastIndexOf('/');
    		if(i >= 0) location = location.substring(i + 1);
    		return location;
    	}
    	return super.getPresentationString();
    }
}

