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

import java.util.*;
import org.jboss.tools.common.model.ui.dnd.IControlDragDropProvider;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.XModelObject;

public class TreeDragDropProvider implements IControlDragDropProvider {
	protected Tree tree;
	
	public TreeDragDropProvider() {}
	
	public void setTree(Tree tree) {
		this.tree = tree;
	}

	public Control getControl() {
		return tree;
	}

	public XModelObject getModelObjectForWidget(Widget widget) {
		if(!(widget instanceof TreeItem)) return null;
		TreeItem item = (TreeItem)widget;
		Object data = item.getData();
		return (!(data instanceof XModelObject)) ? null : (XModelObject)data;
	}

	public Widget[] getSelection() {
		return tree.getSelection();
	}

	public Properties getDropProperties(int x, int y) {
		Properties p = new Properties();
		p.setProperty("actionSourceGUIComponentID", "navigator");
		return p;
	}
}
