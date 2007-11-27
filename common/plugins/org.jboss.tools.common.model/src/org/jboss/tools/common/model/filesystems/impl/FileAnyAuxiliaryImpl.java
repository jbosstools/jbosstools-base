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
import org.jboss.tools.common.model.filesystems.*;
import org.jboss.tools.common.model.util.XModelObjectCache;

public class FileAnyAuxiliaryImpl extends FileAnyImpl {
    private static final long serialVersionUID = 6540080757855996101L;
	private FileAuxiliary helper = null;
    protected XModelObjectCache main = null;
    
    public FileAnyAuxiliaryImpl() {}

    public BodySource getBodySource() {
        if(super.getBodySource() == null && isActive()) {
            FolderLoader fl = (FolderLoader)getParent();
            setBodySource(fl.getBodySource(toFileName(this)));
        }
        return super.getBodySource();
    }

    public void setMainObject(XModelObject main) {
        this.main = new XModelObjectCache(main);
    }
    
    public void setAuxiliaryHelper(FileAuxiliary helper) {
    	this.helper = helper;
    }
    
    public FileAuxiliary getAuxiliaryHelper() {
    	return helper;
    }

    public XModelObject getMainObject() {
        return main == null ? null : main.getObject();
    }

    public boolean isObsolete() {
    	if (main == null) return false;
    	XModelObject o = main.getObject();
    	if(o == null || !o.isActive()) return true;
    	String n = helper.getMainName(this);
        return !n.equals(o.getAttributeValue("name"));
    }

    public void setModified(boolean value) {}

}
