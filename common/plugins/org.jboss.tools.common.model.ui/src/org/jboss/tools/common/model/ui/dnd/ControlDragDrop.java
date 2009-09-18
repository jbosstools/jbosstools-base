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
package org.jboss.tools.common.model.ui.dnd;

import java.util.*;

import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.dnd.*;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.model.options.impl.PaletteAdopt;
import org.jboss.tools.common.model.ui.navigator.TreeViewerDragDropProvider;

import org.jboss.tools.common.meta.XAdoptManager;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelTransferBuffer;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ControlDragDrop {
	static XAdoptManager paletteAdopt;
	
	static {
		XAdoptManager[] ms = new PaletteAdopt().getManagers();
		if(ms != null && ms.length > 0) {
			paletteAdopt = ms[0];			
		}
	}
	
	protected IControlDragDropProvider provider;
	
	public ControlDragDrop() {}
	
	public Control getControl() {
		return provider.getControl();
	}
	public void setProvider(IControlDragDropProvider provider) {
		this.provider = provider;
	}

	protected XModelObject getModelObjectForWidget(Widget widget) {
		return provider.getModelObjectForWidget(widget);
	}
	
	protected XModelObject getLeadSelection() {
		Widget[] w = provider.getSelection();
		return (w == null || w.length == 0) ? null : getModelObjectForWidget(w[0]);
	}
	
	protected XModelObject[] getMultiSelection() {
		Widget[] w = provider.getSelection();
		if(w == null || w.length < 2) return null;
		XModelObject[] os = new XModelObject[w.length];
		for (int i = 0; i < os.length; i++) os[i] = getModelObjectForWidget(w[i]); 
		return os;
	}
	
	public void enable() {
		enableDrag();
		enableDrop();
	}
	
	public void enableDrag() {
		DragSource ds = new DragSource(getControl(), DND.DROP_MOVE | DND.DROP_COPY);
		Transfer[] ts = new Transfer[]{ModelTransfer.getInstance(), TextTransfer.getInstance()};
		ds.setTransfer(ts);
		ds.addDragListener(new DSL());		 
	}
	
	public void enableDrop() {
		DropTarget old = (DropTarget)getControl().getData("DropTarget"); //$NON-NLS-1$
		if(old != null) {
			old.setTransfer(new Transfer[]{ModelTransfer.getInstance(), TextTransfer.getInstance()});
			old.addDropListener(new DTL());
		} else {
			DropTarget target = new DropTarget(getControl(), DND.DROP_MOVE | DND.DROP_COPY);
			Transfer[] types = new Transfer[] {ModelTransfer.getInstance()};
			target.setTransfer(types);
			target.addDropListener(new DTL());
		}
	}
	
	XModelObject draggedObject = null;
	
	class DSL implements DragSourceListener {
		public void dragStart(DragSourceEvent e) {
			XModelObject o = getLeadSelection();
			XModelObject[] targets = (o == null) ? null : getMultiSelection();
			XModelTransferBuffer.getInstance().enable();
			e.doit = (DnDUtil.isCopyEnabled(o, targets) && DnDUtil.copy(o, targets));
			if(e.doit) {
				draggedObject = o;
			} else {
				XModelTransferBuffer.getInstance().disable();
			}
		}
		public void dragSetData(DragSourceEvent event) {
			if (ModelTransfer.getInstance().isSupportedType(event.dataType)) {
				XModelObject o = getLeadSelection();
///				event.data = (o == null) ? new String[0] : new String[]{o.getPresentationString()};
				event.data = (o == null) ? "" : o.getPresentationString(); //$NON-NLS-1$
			} else if(TextTransfer.getInstance().isSupportedType(event.dataType)) {
				XModelObject o = getLeadSelection();
				event.data = (o == null) ? "" : o.getPresentationString(); //$NON-NLS-1$
			}
		}
		public void dragFinished(DragSourceEvent event) {
			XModelTransferBuffer.getInstance().disable();
			if (event.detail == DND.DROP_MOVE) {}
		}
		
	}

	class Scroller implements Runnable {
		private long border_time = -1;
		private int border = -1;
		private long item_time = -1;
		private Widget item = null;
		int status = 0;
		boolean scrollPending = false;
		boolean expandPending = false;
		
		public void run() {
			while(status == 1) {
				try {
					Thread.sleep(200);
				} catch (InterruptedException e) {
					ignore();
				}
				long t = System.currentTimeMillis();
				if(item != null && t > item_time) {
					expandPending = true;
					item_time = -1;
				}
				if(border != -1 && t > border_time) {
					border_time += 500;
					scrollPending = true;
				}
			}
			status = 0;
		}
		
		public void start() {
			if(status == 0) {
				status = 1;
				new Thread(this).start();
			}
		}
		
		public void stop() {
			status = 2;
		}
		
		Widget lastItem = null;
		public void update(Widget w, int x, int y) {
			if (w != null && lastItem != w) lastItem = w;
			if(scrollPending) fireScroll();
			if(expandPending) fireExpand();

			if(w != item) {
				item = w;
				if(item != null) item_time = System.currentTimeMillis() + 1500; 
			}
			Scrollable sc = (provider.getControl() instanceof Scrollable) ? (Scrollable)provider.getControl() : null;
			if(sc == null) return; 

			Rectangle c = sc.getClientArea();
			int yp_t = sc.toDisplay(new Point(c.x, c.y)).y;
			int yp_b = yp_t + c.height;
			if(yp_t < y && yp_t + 20 > y) {
				if(border != 1) {
					border = 1;
					border_time = System.currentTimeMillis() + 500;
				}
			} else if(y < yp_b && y + 20 > yp_b) {
				if(border != 2) {
					border = 2;
					border_time = System.currentTimeMillis() + 500;
				}
			} else {
				if(border != -1) {
					border = -1;
					border_time = -1;
				}
			}
		}
		
		protected void fireExpand() {
			expandPending = false;
			if(!(item instanceof TreeItem)) return;
			if(provider instanceof TreeViewerDragDropProvider) {
				TreeViewerDragDropProvider p = (TreeViewerDragDropProvider)provider;
				p.expand(item);
			}
		}
		
		protected void fireScroll() {
			scrollPending = false;
			if(!(provider.getControl() instanceof Tree) || lastItem == null) return;
				 
			Tree tree = (Tree)provider.getControl();
			TreeItem i = (TreeItem)lastItem;
//			Rectangle r = i.getBounds();
			
			Scrollable sc = (provider.getControl() instanceof Scrollable) ? (Scrollable)provider.getControl() : null;
			if(sc == null || sc.getVerticalBar() == null) return;

			if(border == 1 && sc != null) {
				TreeItem i2 = getPrevTreeItem(tree, i);
				if(i2 != null) tree.showItem(i2);
			} else if(border == 2 && sc != null) {
				TreeItem i2 = getNextTreeItem(tree, i);
				if(i2 != null) tree.showItem(i2);
			}
		}
		
		private TreeItem getNextTreeItem (Tree tree, TreeItem item) {
			TreeItem result = null;
			Vector<TreeItem> rows = getTreeExpandedItems(tree);
			int index = rows.indexOf(item);
			if (index > -1) {
				result = (index + 1 < rows.size() ? rows.get(index + 1) : item);
			} else {
				result = item;
			}
			return result;
		}

		private TreeItem getPrevTreeItem (Tree tree, TreeItem item) {
			TreeItem result = null;
			Vector<TreeItem> rows = getTreeExpandedItems(tree);
			int index = rows.indexOf(item);			
			if (index > 0) {
				result = (TreeItem)rows.get(index - 1);
			} else {
				result = item;
			}
			return result;
		}
	}

	class DTL implements DropTargetListener {
		private boolean isTextDrop = false;
		private boolean isPaletteDrop = false;
		private Scroller scroller = new Scroller();
		private Widget lastItem = null;
		
		public void dragEnter(DropTargetEvent event) {
			scroller.start();
		}
		public void dragOver(DropTargetEvent event) {
			if (event.item != null && lastItem != event.item)
				lastItem = event.item;
			scroller.update(event.item, event.x, event.y);
			
			{
				XModelObject o = getModelObjectForWidget(event.item);
				if(o == null) {
					event.detail = DND.DROP_NONE;
					return;
				}
				boolean enabled = DnDUtil.isPasteEnabled(o);
				isTextDrop = false;
				isPaletteDrop = false;
				Properties p = provider.getDropProperties(event.x, event.y);
				if(p != null && ("none".equals(p.getProperty("text-context")))) { //$NON-NLS-1$ //$NON-NLS-2$
					enabled = false;
				} else if(!enabled && o.isObjectEditable()) {
					if(p != null && "true".equals(p.getProperty("accepsAsString"))) { //$NON-NLS-1$ //$NON-NLS-2$
						if(isAdoptableMacro(o)) {
							isPaletteDrop = true;
						} else {
							isTextDrop = true;
						}
						enabled = true;
					}
				}
				event.detail = (enabled) ? DND.DROP_MOVE : DND.DROP_NONE;
			}
		}
		boolean isAdoptableMacro(XModelObject target) {
			if(paletteAdopt == null) return false;
			if(target.getFileType() != XModelObject.FILE) return false;
			XModelObject source = target.getModel().getModelBuffer().source();
			if(source == null) return false;
			return paletteAdopt.isAdoptable(target, source);
		}
		public void dragLeave(DropTargetEvent event) {
			scroller.stop();
		}
		public void dragOperationChanged(DropTargetEvent event) {};
		public void dropAccept(DropTargetEvent event) {}
		public void drop(DropTargetEvent event) {
			scroller.stop();
//			if(event.currentDataType.type != ModelTransfer.TYPE) {
//				event.detail = DND.DROP_NONE;
//			} else 
			{
				Widget w = event.item;
				if(provider instanceof TreeViewerDragDropProvider) {
					w = lastItem;
				}
				XModelObject o = getModelObjectForWidget(w);
				if(o == null) return;
				try {
					Properties p = provider.getDropProperties(event.x, event.y);
					if(p == null) p = new Properties();
					p.setProperty("isDrop", "true"); //$NON-NLS-1$ //$NON-NLS-2$
					if(isTextDrop) {
						XModelObject s = o.getModel().getModelBuffer().source();
						if(s == null) return;
						p.put("start text", "" + s.getPresentationString()); //$NON-NLS-1$ //$NON-NLS-2$
					} else if(isPaletteDrop) {
						paletteAdopt.adopt(o, o.getModel().getModelBuffer().source(), p);
					} else if(DnDUtil.isPasteEnabled(o)) {
						DnDUtil.paste(o, p);
					}
					if(provider instanceof IControlDropListener) {
						p.put("drop.x", Integer.valueOf(event.x)); //$NON-NLS-1$
						p.put("drop.y", Integer.valueOf(event.y)); //$NON-NLS-1$
						if(draggedObject != null) p.put("draggedObject", draggedObject); //$NON-NLS-1$
						((IControlDropListener)provider).drop(p);
					}
				} catch (ActionDeclinedException ade) {
					ignore();
				} catch (XModelException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				}
			}
		}
	}
	
	void ignore() {
		//do nothing
	}
	
	private static Vector<TreeItem> getTreeExpandedItems(Tree tree) {
		TreeItem[] items = tree.getItems();
		Vector<TreeItem> result = new Vector<TreeItem>();
			
		for (int i = 0; items != null && i < items.length; i++) {
			result.add(result.size(), items[i]);	
			if (items[i].getExpanded()) fillTreeItemRows(items[i].getItems(), result);
		}
		return result;
	}
		
	private static Vector<TreeItem> fillTreeItemRows(TreeItem[] items, Vector<TreeItem> result){ 
		if (result == null) result = new Vector<TreeItem>();
		for (int i = 0; items != null && i < items.length; i++) {
			result.add(result.size(), items[i]);	
			if (items[i].getExpanded()) fillTreeItemRows(items[i].getItems(), result);
		}
		return result;
	}
	
	public Widget findTreeItem (int x, int y) {
		Tree tree = (provider.getControl() instanceof Tree) ? (Tree)provider.getControl() : null;
		Vector<TreeItem> items = getTreeExpandedItems(tree);
		for (int i = 0; i < items.size(); i++) {
			TreeItem item = items.get(i);
			Rectangle bounds = item.getBounds();
			Point p = tree.toDisplay(bounds.x, bounds.y);
			
			if (/*x >= p.x && x <= p.x + bounds.width &&*/
				y >= p.y && y <= p.y + bounds.height) {
					return item;						
				}
		}
		return null;
	}
}
