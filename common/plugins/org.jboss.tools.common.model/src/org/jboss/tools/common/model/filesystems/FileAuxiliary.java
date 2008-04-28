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
package org.jboss.tools.common.model.filesystems;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.*;

public class FileAuxiliary {
    public static String AUX_FILE_ENTITY = "FileAnyAuxiliary";
    String extension = "";
    boolean replaceExtension = true;
    boolean addLeadingDot = true;

    public FileAuxiliary(String extension, boolean replaceExtension) {
    	this.extension = extension;
    	this.replaceExtension = replaceExtension;
   	}

    public String read(XModelObject folder, XModelObject main) {
        FileAnyAuxiliaryImpl f = getAuxiliaryFile(folder, main, false);
        if(f == null) return null;
        BodySource bs = f.getBodySource();
        return (bs == null) ? null : bs.get();
    }

    public boolean write(XModelObject folder, XModelObject main, XModelObject object) {
        FileAnyAuxiliaryImpl f = getAuxiliaryFile(folder, main, true);
        if(f == null) return false;
        BodySource bs = f.getBodySource();
        return (bs != null && bs.write(object));
    }

    public FileAnyAuxiliaryImpl getAuxiliaryFile(XModelObject folder, XModelObject main, boolean create) {
        String name = getAuxiliaryName(main);
        String p = name + "." + extension;
        XModelObject f = folder.getChildByPath(p);
        if(!(f instanceof FileAnyAuxiliaryImpl)) {
            if(!create) return null;
            f = folder.getModel().createModelObject(AUX_FILE_ENTITY, null);
            f.setAttributeValue("name", name);
            f.setAttributeValue("extension", extension);
            folder.addChild(f);
        }
        FileAnyAuxiliaryImpl aux = (FileAnyAuxiliaryImpl)f;
        aux.setMainObject(main);
        aux.setAuxiliaryHelper(this);
        return aux;
    }
    
    public String getMainName(XModelObject aux) {
    	String auxname = aux.getAttributeValue("name");
    	if(replaceExtension) return auxname;
		int i = auxname.lastIndexOf('.');
		return (i < 0) ? auxname : auxname.substring(0, i);
    }
    
    public String getAuxiliaryName(XModelObject main) {
		String name = main.getAttributeValue("name");
		if(!replaceExtension) name += "." + main.getAttributeValue("extension");
		if(addLeadingDot) name = "." + name;
		return name;
    }
    
}
