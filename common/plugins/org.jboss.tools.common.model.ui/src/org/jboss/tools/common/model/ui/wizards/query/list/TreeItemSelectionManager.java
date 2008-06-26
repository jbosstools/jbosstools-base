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
package org.jboss.tools.common.model.ui.wizards.query.list;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.TreeItem;

public class TreeItemSelectionManager {
	public interface Listener {
		public void flip(TreeItem item);
		public boolean isSelected(Object data);
	}
	
	TreeViewer treeViewer = null;
	Listener listener = null;

	public TreeItemSelectionManager(TreeViewer treeViewer, Listener listener) {
		this.listener = listener;	
		this.treeViewer = treeViewer;
		update();
		treeViewer.getTree().addKeyListener(new KL());
		treeViewer.getTree().addSelectionListener(new SL());
		treeViewer.getTree().addTreeListener(new EL());
	}

	int lock = 0;
	class EL implements TreeListener {

		public void treeCollapsed(TreeEvent e) {
		}

		public void treeExpanded(TreeEvent e) {
			if(e.item instanceof TreeItem) {
				TreeItem item = (TreeItem)e.item;
				TreeItem[] is = item.getItems();
				update(is);
			}			
		}		
	}
	
	public void update() {
		update(treeViewer.getTree().getItems());
	}
	
	void update(TreeItem[] is) {
		lock++;
		try {
			for (int i = 0; i < is.length; i++) {
				Object d = is[i].getData();
				is[i].setChecked(listener.isSelected(is[i].getData()));
				update(is[i].getItems());
			}
		} finally {
			lock--;
		}
	}
	
	class SL implements SelectionListener {

		public void widgetDefaultSelected(SelectionEvent e) {
			widgetSelected(e);
		}

		public void widgetSelected(SelectionEvent e) {
			if(e.item instanceof TreeItem) {
				if(lock > 0) return;
				lock++;
				try {
					TreeItem item = (TreeItem)e.item;
					if(listener.isSelected(item.getData()) != item.getChecked()) {
						listener.flip(item);
					}
					if(listener.isSelected(item.getData()) != item.getChecked()) {
						item.setChecked(listener.isSelected(item.getData()));
					}
				} finally {
					lock--;
				}
			}
			
		}
		
	}

	class KL extends KeyAdapter {
		public void keyPressed(KeyEvent e) {
			boolean flip = e.keyCode == (int)' ';
			if(flip) {
				TreeItem[] items = treeViewer.getTree().getSelection();
				TreeItem item = (items == null || items.length == 0) ? null : items[0];
				listener.flip(item);
			}
		}
	}
}
