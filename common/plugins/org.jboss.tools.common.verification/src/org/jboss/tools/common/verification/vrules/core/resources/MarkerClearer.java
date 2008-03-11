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
package org.jboss.tools.common.verification.vrules.core.resources;

import org.eclipse.core.resources.IProject;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.IModelNature;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class MarkerClearer {
	
	public static void clear(XModelObject o) {
		if(o == null || o.getFileType() == XModelObject.NONE) return;
		if(o.getFileType() == XModelObject.FILE) {
			ResourceMarkers markers = new ResourceMarkers(ResourceMarkers.JST_WEB_PROBLEM);
			markers.setModelObject(o);
			markers.update();
		} else {
			XModelObject[] os = o.getChildren();
			for (int i = 0; i < os.length; i++) clear(os[i]); 
		}
	}
	
	public static void clearAll() {
		IProject[] ps = ModelPlugin.getWorkspace().getRoot().getProjects();
		for (int i = 0; i < ps.length; i++) {
			IModelNature n = (IModelNature)EclipseResourceUtil.getModelNature(ps[i]);
			if(n == null) continue;
			try {
				XModelObject o = FileSystemsHelper.getWebInf(n.getModel());
				clear(o);				
			} catch (Exception e) {}
		}
	}
	
//	private static String getModelNature() {
//		return "";
//	}
	
}
