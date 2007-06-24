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
package org.jboss.tools.common.model.java.handlers;

import java.util.Properties;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.ui.actions.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class MoveHandler extends AbstractHandler {
	public boolean isEnabled(XModelObject object) {
		return object != null;
	}
	 
	public void executeHandler(XModelObject object, Properties p) throws Exception {
		SiteWrapper site = new SiteWrapper(ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart().getSite());
		SelectionDispatchAction action = createAction(site);
		run(site, action, object);
	}
	
	protected SelectionDispatchAction createAction(SiteWrapper site) {
		return new MoveAction(site);
	}
	
	protected void run(SiteWrapper site, SelectionDispatchAction action, XModelObject object) {
    	IResource file = EclipseResourceUtil.getResource(object);
    	action.selectionChanged(new SelectionChangedEvent(ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart().getSite().getSelectionProvider(), new StructuredSelection(file)));
    	if(file != null) {
    		Object so = EclipseResourceUtil.findJavaElement(object);
    		if(so == null) so = file;
    		site.setSelection(new StructuredSelection(so));
    		action.run(new StructuredSelection(so));
    	} else {
    		action.run();
    	}
		object.getModel().update();
	}
	
	protected class SiteWrapper implements IWorkbenchSite, ISelectionProvider {
		IWorkbenchSite site;
		ISelection selection;
		
		public SiteWrapper(IWorkbenchSite site) {
			this.site = site;
		}
		public IWorkbenchPage getPage() {
			return site.getPage();
		}
		public ISelectionProvider getSelectionProvider() {
			return selection == null ? site.getSelectionProvider() : this;
		}
		public Shell getShell() {
			return site.getShell();
		}
		public IWorkbenchWindow getWorkbenchWindow() {
			return site.getWorkbenchWindow();
		}
		public void setSelectionProvider(ISelectionProvider provider) {
		}
		public Object getAdapter(Class adapter) {
			return site.getAdapter(adapter);
		}
		public void addSelectionChangedListener(ISelectionChangedListener listener) {
		}
		public ISelection getSelection() {
			return selection;
		}
		public void removeSelectionChangedListener(ISelectionChangedListener listener) {
		}
		public void setSelection(ISelection selection) {
			this.selection = selection;
		}
		public Object getService(Class api) {
			return null;
		}
		public boolean hasService(Class api) {
			return false;
		}
	}
	
}
