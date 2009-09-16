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

public class FileSystemsTreeConstraint implements XFilteredTreeConstraint {

    public FileSystemsTreeConstraint() {}

    public void update(XModel model) {}
    
    String excludeExtensions = ".project.classpath."; //$NON-NLS-1$

    public boolean accepts(XModelObject object) {
        String entity = object.getModelEntity().getName();
        if("FileAnyAuxiliary".equals(entity)) return false; //$NON-NLS-1$
        if(object.getFileType() == XFileObject.SYSTEM) {
            String s = object.getAttributeValue("info"); //$NON-NLS-1$
            return (s == null || s.indexOf("hidden=yes") < 0); //$NON-NLS-1$
        }
        if(object.getFileType() == XFileObject.FILE) {
        	String ext = "." + object.getAttributeValue(XModelObjectConstants.ATTR_NAME_EXTENSION) + "."; //$NON-NLS-1$ //$NON-NLS-2$
        	if(excludeExtensions.indexOf(ext) >= 0) return false;
        	if(".rule-sets".equals(object.getAttributeValue(XModelObjectConstants.ATTR_NAME))) return false;        	 //$NON-NLS-1$
        } else if(object.getFileType() == XFileObject.FOLDER) {
			String[] ns = getHiddenFolderNames();
			String n = object.get(XModelObjectConstants.XML_ATTR_NAME);
			for (int i = 0; i < ns.length; i++) if(ns[i].equalsIgnoreCase(n)) return false;
        }
        return true;
    }

    public boolean isHidingAllChildren(XModelObject object) {
        if(object.getFileType() > XFileObject.NONE
           && XModelObjectConstants.TRUE.equals(object.get("overlapped"))) return true; //$NON-NLS-1$
//        String entity = object.getModelEntity().getName();
        return false; ///entity.equals("FilePROPERTIES");
    }

    public boolean isHidingSomeChildren(XModelObject object) {
        if(object.getModelEntity().getName().equals(FileSystemsHelper.FILE_SYSTEMS)) return true;
        if(object.getFileType() < XFileObject.FOLDER) return false;
        return true;
    }

    private static String[] names = new String[]{"CVS", ".svn"}; //$NON-NLS-1$ //$NON-NLS-2$

    protected String[] getHiddenFolderNames() {
        return names;
    }

}

