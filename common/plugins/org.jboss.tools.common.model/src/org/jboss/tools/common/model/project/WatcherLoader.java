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
package org.jboss.tools.common.model.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.QualifiedName;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.*;

public class WatcherLoader implements XObjectLoader {
	public static QualifiedName LOCK = new QualifiedName("", "lock-model");

	public void load(XModelObject object) {
		if(EclipseResourceUtil.isProjectFragment(object.getModel())) return;
        if(isLocked(object.getModel())) return;
        Watcher.getInstance(object.getModel()).forceUpdate();
    }

    public boolean update(XModelObject object) {
        return true;
    }

    public boolean save(XModelObject object) {
        return true;
    }
    
    public static boolean isLocked(XModel model) {
    	IProject p = EclipseResourceUtil.getProject(model.getRoot());
    	if(p == null) return false;
    	try {
    		return "true".equals(p.getSessionProperty(LOCK));
    	} catch(Exception e) {
    		//ignore
    		return false;
    	}
    }

}
