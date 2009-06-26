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
package org.jboss.tools.common.model.ui.views.navigator;

import java.util.Properties;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.navigator.TreeViewerMenuInvoker;
import org.jboss.tools.common.model.util.FindObjectHelper;

public class NavigatorMenuInvoker extends TreeViewerMenuInvoker {
	private static XModelObject eclipseWorkspace = null;

	public XModelObject getModelObjectAt(Point p) {
		XModelObject o = super.getModelObjectAt(p);
		return (o != null) ? o : getWorkspaceObject();
	}
	
	protected XModelObject getWorkspaceObject() {
		if(eclipseWorkspace == null) {
			XModel model = PreferenceModelUtilities.getPreferenceModel();
			if(model != null) {
				eclipseWorkspace = model.createModelObject("EclipseWorkspace", null); //$NON-NLS-1$
			}
		}
		return eclipseWorkspace;
	}
	
	public void mouseDoubleClick(MouseEvent e) {
		XModelObject o = getModelObjectAt(new Point(e.x, e.y));
		if (o == null) return;
		if(XActionInvoker.getAction("Open", o) != null) { //$NON-NLS-1$
			XActionInvoker.invoke("Open", o, new Properties()); //$NON-NLS-1$
		} else if(o.getFileType() == XModelObject.NONE) {
	        FindObjectHelper.findModelObject(o, FindObjectHelper.IN_EDITOR_ONLY);
		}
	}

	protected void handleMouseUp(MouseEvent e) {
		if(e.button == 1 && isOpenOnSingleClick()) {
			mouseDoubleClick(e);
		} else {
			super.handleMouseUp(e);
		}		
	}
	
	private boolean isOpenOnSingleClick() {
		return Platform.getPreferencesService().getBoolean("org.eclipse.ui.workbench", "OPEN_ON_SINGLE_CLICK", true, new IScopeContext[]{new InstanceScope()});  //$NON-NLS-1$ //$NON-NLS-2$
	}

	protected void fillRunningProperties(Properties p) {
		p.setProperty("actionSourceGUIComponentID", "navigator"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
}
