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
package org.jboss.tools.common.model.ui.outline;

import java.util.*;

import org.jboss.tools.common.model.ui.navigator.TreeViewerMenuInvoker;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.Point;

import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.model.XModelObject;

public class OutlineMenuInvoker extends TreeViewerMenuInvoker {

	public XActionList getActionList(XModelObject o) {
		if(o.getFileType() != XModelObject.FILE) return super.getActionList(o);
		String ent = o.getModelEntity().getName() + "_EditorActionList";
		XModelEntity entity = o.getModel().getMetaData().getEntity(ent); 
		return (entity != null) ? entity.getActionList() : super.getActionList(o);
	}

	public void mouseDoubleClick(MouseEvent e) {
		XModelObject o = getModelObjectAt(new Point(e.x, e.y));
		if (o == null || o.getFileType() != XModelObject.NONE) return;
		if(XActionInvoker.getAction(o.getModelEntity(), "Open") != null) {
			XActionInvoker.invoke("Open", o, new Properties());
		} else if(XActionInvoker.getAction(o.getModelEntity(), "Properties.Properties") != null) {
			XActionInvoker.invoke("Properties.Properties", o, new Properties());
		}
	}
	
	protected void fillRunningProperties(Properties p) {
		p.setProperty("actionSourceGUIComponentID", "editor");
	}
}
