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

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Widget;

public class TreeViewerDragDropProvider extends TreeDragDropProvider {
	TreeViewer viewer;
	
	public void setViewer(TreeViewer viewer) {
		this.viewer = viewer;
		setTree(viewer.getTree());	
	}
	
	public void expand(Widget w) {
		Object o = getModelObjectForWidget(w);
		if(o != null) viewer.expandToLevel(o, 1);
	}

}
