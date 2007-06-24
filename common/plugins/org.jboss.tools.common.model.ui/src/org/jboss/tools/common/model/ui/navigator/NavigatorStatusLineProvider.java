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
package org.jboss.tools.common.model.ui.navigator;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.*;

public class NavigatorStatusLineProvider {
	
	public String getStatusLineMessage(IStructuredSelection selection) {
		if (selection.size() == 1) {
			Object o = selection.getFirstElement();
			if (o instanceof XModelObject) {
				return getStatusLineMessage((XModelObject)o);
			} else {
				return "";
			}
		}
		if (selection.size() > 1) {
			return "" + selection.size() + " items selected";
		}
		return "";
	}
	
	protected String getStatusLineMessage(XModelObject object) {
		if(object.getFileType() > XModelObject.NONE) {
			return getStatusLineMessageForResource(object);
		} else {
			return getStatusLineMessageForFileFragment(object);
		}
	}
	
	private String getStatusLineMessageForResource(XModelObject object) {
		IResource resource = EclipseResourceUtil.getResource(object);
		String msg = (resource == null) ? "" : resource.getFullPath().makeRelative().toString();
		if(msg.length() == 0 && object.getFileType() == XModelObject.SYSTEM) {
			msg = XModelObjectUtil.expand("" + object.get("LOCATION"), object.getModel(), null);
		}
		return (msg.length() == 0) ? object.getPathPart() : msg;
	}
	
	private String getStatusLineMessageForFileFragment(XModelObject object) {
		if(object == null) return "";
		if(!object.isActive()) return object.getPresentationString();
		XModelObject f = object.getParent();
		while(f != null && f.getFileType() == XModelObject.NONE) f = f.getParent();
		if(f == null) return object.getPath().replace('/', '.');
		StringBuffer sb = new StringBuffer();
		XModelObject p = object;
		while(p != null && p != f) {
			if(sb.length() > 0) sb.insert(0, '.');
			sb.insert(0, p.getPresentationString());
			p = p.getParent(); 
		}
		String relative = sb.toString();
		String parent = getStatusLineMessageForResource(f);
		return relative + " - " + parent;
	}

}
