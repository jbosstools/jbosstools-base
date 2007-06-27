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
package org.jboss.tools.common.model.ui.action;

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

import org.eclipse.core.resources.IResource;

public abstract class AbstractModelActionDelegate implements IWorkbenchWindowActionDelegate {
	protected XModelObject object = null;
	protected IWorkbenchWindow window;

	public void init(IWorkbenchWindow window) {
		this.window = window;
	}

	public void selectionChanged(final IAction action, final ISelection selection) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				safeSelectionChanged(action, selection);
			}
		});
	}
	
	protected void safeSelectionChanged(IAction action, ISelection selection) {
		if(object == null && action.isEnabled()) action.setEnabled(false);
		XModelObject adapter = getAdapter(selection);
		if(adapter == null) return;
		object = adapter;
		action.setEnabled(computeEnabled());	
	}
	
	protected final XModelObject getAdapter(ISelection selection) {
		if(!(selection instanceof IStructuredSelection)) return null;
		Object o = ((IStructuredSelection)selection).getFirstElement();
		if(o instanceof IResource) {
			return EclipseResourceUtil.getObjectByResource((IResource)o);
		} else if(o instanceof IJavaElement) {
			return EclipseResourceUtil.getObjectByResource(((IJavaElement)o).getJavaProject().getProject());
		}
		return (o instanceof XModelObject) ? (XModelObject)o : null;
	}
	
	protected abstract boolean computeEnabled(); 

	public void run(IAction action) {
		try {
			doRun();
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}
	
	protected abstract void doRun() throws Exception;

	public void dispose() {}
	
}
