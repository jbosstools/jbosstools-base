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
package org.jboss.tools.common.meta.ui.search;

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.search.ui.*;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class MetaSearchQuery implements ISearchQuery {
	MetaSearchResult result;
	String text = "=";
	boolean ignoreCase = true;
	String attributeMask = "*";
	List scope = new ArrayList();
	
	public void setScope(List scope) {
		this.scope = scope;
	}
	
	public void setTextToFind(String t) {
		text = t;
	}
	
	public void setIgnoreCase(boolean b) {
		ignoreCase = b;
	}
	
	public void setAttributeMask(String m) {
		attributeMask = m;
	}
	
	public String getTextToFind() {
		return text;
	}

	public IStatus run(final IProgressMonitor monitor) {
		final MetaSearchResult textResult= (MetaSearchResult)getSearchResult();
		textResult.removeAll();
		///create results
		IResource[] rs = (IResource[])scope.toArray(new IResource[0]);
		for (int i = 0; i < rs.length; i++) {
			processResource(rs[i]);
		}
		return new Status(IStatus.OK, "org.jboss.common.meta.ui", 0, "", null);
	}
	
	private void processResource(IResource resource) {
		if(resource instanceof IContainer) {
			IContainer c = (IContainer)resource;
			IResource[] rs = null;
			try { rs = c.members(); } catch (Exception e) {}
			if(rs != null) for (int i = 0; i < rs.length; i++) processResource(rs[i]);
		} else if(resource instanceof IFile) {
			IFile f = (IFile)resource;
			if(!f.getName().endsWith(".meta")) return;
			XModelObject o = EclipseResourceUtil.getObjectByResource(f);
			if(o == null) o = EclipseResourceUtil.createObjectForResource(f);
			if(o == null) return;
			processObject(o);
		}
	}
	
	private void processObject(XModelObject o) {
		XAttribute[] as = o.getModelEntity().getAttributes();
		for (int i = 0; i < as.length; i++) {
			if(!as[i].isVisible()) continue;
			String n = as[i].getName();
			if(!attributeMask.equals("*") && attributeMask.length() > 0 && !attributeMask.equals(n)) continue;
			String v = o.getAttributeValue(n);
			if(v == null) continue;
			if(ignoreCase) {
				if(v.toLowerCase().indexOf(text.toLowerCase()) < 0) continue;
			} else {
				if(v.indexOf(text) < 0) continue;
			}
			result.addObject(o);
			break;
		}
		XModelObject[] cs = o.getChildrenForSave();
		for (int i = 0; i < cs.length; i++) {
			processObject(cs[i]);
		}
	}

	public String getLabel() {
		return "Meta Search";
	}

	public boolean canRerun() {
		return true;
	}

	public boolean canRunInBackground() {
		return true;
	}

	public ISearchResult getSearchResult() {
		if(result == null) {
			result = new MetaSearchResult();
			result.setQuery(this);
		}
		return result;
	}

}
