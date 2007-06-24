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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.TreeItem;

public class TreeItemSelectionManager {
	public interface Listener {
		public void flip(TreeItem item);
	}
	
	TreeViewer treeViewer = null;
	Listener listener = null;

	public TreeItemSelectionManager(TreeViewer treeViewer, Listener listener) {
		this.listener = listener;	
		this.treeViewer = treeViewer;
		SL sl = new SL();
		treeViewer.getTree().addMouseListener(sl);
		treeViewer.getTree().addMouseMoveListener(sl);
		treeViewer.getTree().addKeyListener(new KL());
	}

	class SL extends MouseAdapter implements MouseMoveListener {
		TreeItem item = null;
		boolean out = false;
		public void mouseDown(MouseEvent e) {
			item = getItem(e);
			out = false;
		}
		public void mouseUp(MouseEvent e) {
			if(item != null && item == getItem(e) && !out) {
				listener.flip(item);
			}
			item = null;
		}
		public void mouseMove(MouseEvent e) {
			if(item == null) return;
			out = (item != getItem(e));			
		}
		private TreeItem getItem(MouseEvent e) {
			if(e.button != 1) return null;
			TreeItem item = treeViewer.getTree().getItem(new Point(e.x, e.y));
			if(item == null) return null;
			Rectangle r = item.getBounds();
			if(e.x < r.x - 20 || e.x > r.x) return null;
			return item;
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
