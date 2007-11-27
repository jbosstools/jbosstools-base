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
package org.jboss.tools.common.model.filesystems.impl.handlers;

import java.util.Properties;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.ui.actions.RenameAction;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchSite;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class RenameEclipseFileHandler extends AbstractHandler {

    public boolean isEnabled(XModelObject object) {
        return object != null && object.isObjectEditable();
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
    	final IResource file = EclipseResourceUtil.getResource(object);
    	if(file == null) {
    		XActionInvoker.invoke("EditActions.Rename", object, p);
    		return;
    	}
    	Display.getDefault().asyncExec(new Runnable() {
    		public void run() {
    	    	IWorkbenchSite site = ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart().getSite();
    	    	RenameAction renameAction = new RenameAction(site);
    	    	renameAction.run(new StructuredSelection(file));
    		}
    	});
    }

}
