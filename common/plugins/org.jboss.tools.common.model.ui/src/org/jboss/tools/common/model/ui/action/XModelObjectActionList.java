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

import org.eclipse.core.resources.IResource;
import org.eclipse.swt.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.actions.OpenWithMenu;

import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class XModelObjectActionList extends XModelObjectActionItem {
	protected XActionList list;
	
	public XModelObjectActionList(XActionList list, XModelObject object, XModelObject[] targets, Object environment) {
		super((XActionItem)list, object, targets, environment);
		this.list = list;
	}
	
	public void createMenu(Menu parent) {
		if(list.getGroupFactor() == XActionList.DIVISION) {
			if(parent.getItems().length > 0) {
				if(parent.getItems()[parent.getItems().length - 1].getStyle() != SWT.SEPARATOR) 
				  new MenuItem(parent, SWT.SEPARATOR);
			}
			fillMenu(parent);		
		} else {
			MenuItem item = new MenuItem(parent, SWT.CASCADE);
			item.setText(list.getDisplayName());
			Menu menu = new Menu(item);
			item.setMenu(menu);
			fillMenu(menu);
			removeLastSeparator(menu);
			if(menu.getItems().length == 0) {
				item.dispose(); 
			} else {
				menu.setVisible(true);		
			}
		}
	}

	public Menu createMenu(Control parent) {
		Menu menu = new Menu(parent);
		setShell(parent.getShell());
		fillMenu(menu);
		removeLastSeparator(menu);
		return menu;		
	}
	
	public void removeLastSeparator(Menu menu) {
		int c = menu.getItems().length - 1;
		if(c >= 0 && menu.getItems()[c].getStyle() == SWT.SEPARATOR)
			menu.getItems()[c].dispose();
	}
	
	public void fillMenu(Menu menu) {
		XActionItem[] is = list.getActionItems();
		for (int i = 0; i < is.length; i++) {
			if(is[i] instanceof XAction) {
				XAction action = (XAction)is[i];
				XModelObjectAction a = new XModelObjectAction(action, object, targets, environment);
				a.setShell(shell);
				a.createMenuItem(menu);
			} else {
				XActionList l = (XActionList)is[i];
				if ("OpenWith".equals(l.getName())) {
					if (object.getFileType() == XFileObject.FILE && (targets == null || targets.length < 2)) {
						IResource resource = EclipseResourceUtil.getResource(object);
						if (resource != null && resource.getType() == IResource.FILE) {
							MenuItem item = new MenuItem(menu, SWT.CASCADE);
							item.setText(l.getDisplayName());
							Menu subMenu = new Menu(item);
							OpenWithMenu openMenu = new OpenWithMenu(
							ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage(),
								resource
							);
							openMenu.fill(subMenu, 0); 
							item.setMenu(subMenu);
						}
					}
				} else {
					XModelObjectActionList a = new XModelObjectActionList(l, object, targets, environment);
					a.setShell(shell);
					a.createMenu(menu);
				}
			}
		}
	}

}
