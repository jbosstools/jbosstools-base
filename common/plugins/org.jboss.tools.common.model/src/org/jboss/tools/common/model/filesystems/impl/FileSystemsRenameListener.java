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

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.util.XModelObjectUtil;

public class FileSystemsRenameListener {
	static Contribution contribution;
	
	static {
		try {
			contribution = (Contribution)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.jst.web.project.FileSystemsRenameListenerContribution");
		} catch (Exception e) {
			//ignore
		}
	}
	XModelObject fileSystems;
	
	public interface Contribution {
		public void pathRenamed(FileSystemsRenameListener listener, String oldPath, String newPath);
	}
	
	public FileSystemsRenameListener(XModelObject fileSystems) {
		this.fileSystems = fileSystems;
	}
	
	public XModelObject getFileSystems() {
		return fileSystems;
	}

	public void checkFileSystemRename(IResourceChangeEvent event) {
		String[] r = processDelta(event.getDelta());
		if(r == null) return;
		pathRenamed(r[0], r[1]);
	}
	
	private String[] processDelta(IResourceDelta delta) {
		if(delta == null) return null;
		String[] r = extractRename(delta);
		if(r != null) return r;
		IResourceDelta[] cs = delta.getAffectedChildren();
		if(delta.getKind() == IResourceDelta.CHANGED) {
			if(cs != null && cs.length > 0) {
				for (int i = 0; i < cs.length; i++) {
					r = processDelta(cs[i]);
					if(r != null) return r;
				}
			}
		}
		return null;
	}
	
	private String[] extractRename(IResourceDelta delta) {
		if(delta.getKind() != 4) return null;
		IResourceDelta[] cs = delta.getAffectedChildren();
		if(cs == null ||  cs.length != 2) return null;
		String[] r = extractRename(cs[0], cs[1]);
		if(r == null) r = extractRename(cs[1], cs[0]);
		return r;		
	}

	private String[] extractRename(IResourceDelta d1, IResourceDelta d2) {
		if(d1.getKind() != 1 || d2.getKind() != 2) return null;
		IPath from = d1.getMovedFromPath();
		IPath to = d2.getMovedToPath();
		if(from == null || to == null) return null;
		IPath f = getLocation(from);
		IPath t = getLocation(to);
		if(f != null && t != null) {
			from = f;
			to = t;
		}
		return new String[]{from.toString(), to.toString()};
	}
	
	private IPath getLocation(IPath path) {
		if(path.segmentCount() > 1) {
			IFolder folder = null;
			try {
				folder = ModelPlugin.getWorkspace().getRoot().getFolder(path);
			} catch (Exception e) {
				ModelPlugin.log("FileSystemsRenameListener:getLocation: Cannot find folder " + path);
			}
			return folder == null ? null : folder.getLocation();
		} else {
			IProject project = null;
			try {
				project = ModelPlugin.getWorkspace().getRoot().getProject(path.segments()[0]);
			} catch (Exception e) {
				ModelPlugin.log("FileSystemsRenameListener:getLocation: Cannot find project " + path);
			}
			return project == null ? null : project.getLocation();
		}
	}
	
	private void pathRenamed(String oldPath, String newPath) {
		String ws = XModelConstants.getWorkspace(fileSystems.getModel());
		ws = ws.replace('\\', '/');
		if(!ws.toLowerCase().startsWith(oldPath.toLowerCase())) {
			// now ignore
			return;
		}

		if(contribution != null) contribution.pathRenamed(this, oldPath, newPath);
				
		ws = newPath + ws.substring(oldPath.length());
		fileSystems.getModel().getProperties().setProperty(XModelConstants.WORKSPACE, ws);

		//now just recreate folder file systems
		XModelObject[] fs = fileSystems.getChildren("FileSystemFolder");
		for (int i = 0; i < fs.length; i++) {
			String s = XModelObjectUtil.getExpandedValue(fs[i], "location", null);
			s = s.replace('\\', '/');
//			String name = fs[i].getAttributeValue("name");
			XModelObject ns = fs[i].copy(0);
///			ns.setAttributeValue("location", newPath);
			fs[i].removeFromParent();
			fileSystems.addChild(ns);
			((XModelImpl)fileSystems.getModel()).fireStructureChanged(ns);
		}		
		fileSystems.setModified(true);
		fileSystems.getModel().save();
	}

}
