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

import java.util.*;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class ClassPathUpdateWizard implements SpecialWizard {
	ClassPathUpdate u;

	public void setObject(Object object) {
		u = new ClassPathUpdate();
		Properties p = (Properties)object;
		XModel model = (XModel)p.get("model");
		u.setProject((IProject)model.getProperties().get("project"));
		u.setModel(model);
		u.setReplacedSrc((Map)p.get("replacedSrc"));
		IPath classes = (IPath)p.get("classes");
		if(classes != null) u.setClasses(classes);
	}

	public int execute() {
		try {
			u.execute();
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		return 0;
	}

}
