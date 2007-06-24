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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.ui.viewers.xpl.CheckStateChangedEvent;
import org.jboss.tools.common.model.ui.viewers.xpl.ICheckStateListener;
import org.jboss.tools.common.model.ui.viewers.xpl.ICheckable;
import org.jboss.tools.common.model.util.EclipseResourceUtil;


/**
 * @author au
 */

public class CheckTreeAdapter extends DefaultValueAdapter implements ITreeContentProvider {
	
	protected CheckTree tree;
	protected CheckLabelProvider labelProvider;
	
	protected ISelectionChangedListener selectionChangedListener;
	
	private Image IMAGE_CHECK = EclipseResourceUtil.getImage("images/common/check.gif");
	private Image IMAGE_UNCHECK = EclipseResourceUtil.getImage("images/common/uncheck.gif");
	private Image IMAGE_HALFCHECK = EclipseResourceUtil.getImage("images/common/halfcheck.gif");
	
	public class CheckTree implements ICheckable {
		private ArrayList<ICheckStateListener> listeners = new ArrayList<ICheckStateListener>();
		private ArrayList<CheckItem> roots;
		
		public CheckTree() {
			roots = new ArrayList<CheckItem>();
		}
		
		public void dispose() {
			listeners.clear();
		}

		public CheckItem[] getRoot() {
			return (CheckItem[])roots.toArray(new CheckItem[roots.size()]);
		}
		
		public void addRoot(CheckItem child) {
			roots.add(child);
			child.setTree(this);
		}
		
		public void removeRoot(CheckItem child) {
			roots.remove(roots.indexOf(child));
			child.setTree(null);
		}

		public boolean hasChildren(CheckItem element) {
			return (roots.size()>0);
		}

		public void addCheckStateListener(ICheckStateListener listener) {
			listeners.add(listener);
		}

		public int getState(Object element) {
			return ((CheckItem)element).getState();
		}

		public void removeCheckStateListener(ICheckStateListener listener) {
			listeners.remove(listener);
		}

		public boolean setState(Object element, int state) {
			CheckItem item = ((CheckItem)element);
			int oldState = item.getState();
			item.setState(state);
			if (oldState!=state) {
				this.fireStateChanged(item);
			}
			return Boolean.TRUE.booleanValue();
		}

		protected void fireStateChanged(CheckItem item) {
			Iterator i = listeners.iterator();
			CheckStateChangedEvent event = new CheckStateChangedEvent(this, item, item.getState());
			while (i.hasNext()) {
				((ICheckStateListener)i.next()).checkStateChanged(event);
			}
		}

		public void toggle(Object element) {
		}
	}
	
	public class CheckItem implements ICheckable {
		private ArrayList<CheckItem> childs;
		private CheckItem parent;
		private int state;
		private int userState;
		private String name;
		private CheckTree tree;
		private Object data;
		
		private CheckItem() {}
		
		public CheckItem(String name, int state) {
			childs = new ArrayList<CheckItem>();
			parent = null;
			this.name = name;
			this.state = state;
			this.userState = state;
		}

		public CheckItem(String name, int state, CheckItem parent) {
			childs = new ArrayList<CheckItem>();
			//this.parent = parent;
			this.name = name;
			this.state = state;
			this.userState = state;
			if (parent!=null) {
				parent.addChild(this);
			}
		}

		public CheckItem(String name, int state, Object data) {
			childs = new ArrayList<CheckItem>();
			parent = null;
			this.name = name;
			this.state = state;
			this.data = data;
			this.userState = state;
		}

		public CheckItem(String name, int state, Object data, CheckItem parent) {
			childs = new ArrayList<CheckItem>();
			//this.parent = parent;
			this.name = name;
			this.state = state;
			this.data = data;
			this.userState = state;
			if (parent!=null) {
				parent.addChild(this);
			}
		}

		public void addChild(CheckItem child) {
			childs.add(child);
			child.setParent(this);
			if (childs.size()==1) {
				this.setState(child.getState());
			}
			if (this.getState()!=ICheckable.STATE_HALFCHECK && this.getState() != child.getState()) {
				this.setState(ICheckable.STATE_HALFCHECK);
			}
		}
		
		public void removeChild(CheckItem child) {
			childs.remove(childs.indexOf(child));
			child.setParent(null);
		}

		public CheckItem[] getChildrens() {
			return childs.toArray(new CheckItem[childs.size()]);
		}
		
		public boolean hasChildren() {
			return (childs.size()>0);
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public CheckItem getParent() {
			return parent;
		}

		public void setParent(CheckItem parent) {
			this.parent = parent;
		}

		public int getState() {
			return state;
		}

		public void setState(int state) {
			this.state = state;
		}

		public void addCheckStateListener(ICheckStateListener listener) {
		}

		public int getState(Object element) {
			if (this.tree!=null) return tree.getState(element);
			return ICheckable.STATE_UNCHECK;
		}

		public void removeCheckStateListener(ICheckStateListener listener) {
		}

		public boolean setState(Object element, int state) {
			if (this.tree!=null) return tree.setState(element, state);
			// else
			this.setState(state);
			return Boolean.FALSE.booleanValue();
		}
		
		public CheckTree getTree() {
			return tree;
		}

		public void setTree(CheckTree tree) {
			this.tree = tree;
			if (this.hasChildren()) {
				Iterator<CheckItem> i = childs.iterator();
				while (i.hasNext()) i.next().setTree(tree);
			}
		}
		
		protected void toState(int nextState) {
			if (nextState == ICheckable.STATE_CHECK) {
				this.setState(this, ICheckable.STATE_CHECK);
				if (this.hasChildren()) {
					CheckItem[] childs = this.getChildrens();
					for (int i=0;i<childs.length;++i) childs[i].toState(nextState);
				}
			} else if (nextState == ICheckable.STATE_UNCHECK) {
				this.setState(this, ICheckable.STATE_UNCHECK);
				if (this.hasChildren()) {
					CheckItem[] childs = this.getChildrens();
					for (int i=0;i<childs.length;++i) childs[i].toState(nextState);
				}
			} else {
				if (!this.hasChildren()) {
					this.setState(this, this.getUserState());
				} else {
					this.setUserState(nextState);
					if (this.hasChildren()) {
						CheckItem[] childs = this.getChildrens();
						for (int i=0;i<childs.length;++i) childs[i].toState(nextState);
					}
				
					boolean skipHalf = Boolean.TRUE.booleanValue();
					CheckItem[] childs = this.getChildrens();
					int childUserState = childs[0].getUserState();
					for (int i=1;i<childs.length;++i) {
						if (childs[i].getUserState() == ICheckable.STATE_HALFCHECK) {
							skipHalf = Boolean.FALSE.booleanValue();
							break;
						}
						if (childUserState != childs[i].getUserState()) {
							skipHalf = Boolean.FALSE.booleanValue();
							break;
						}
					}
					if (skipHalf) {
						this.setState(this, childUserState);
					} else {
						this.setState(this, nextState);
					}
				}
			}
		}
		
		public void toggle(Object element) {
			if (element==this) {
				// message from control about user change this node
				// 1. get next state (to check, to uncheck, or to user state)
				int nextState = getNextState();
				// 2. notify self and children
				toState(nextState);
				Iterator i = this.childs.iterator();
				while (i.hasNext()) ((CheckItem)i.next()).toState(nextState);
				// 3. notify parent
				if (parent!=null) {
					parent.toggle(element);
				}
			} else {
				// message from child about user change child node
				// 1. copy state to user state for children
				CheckItem item;
				Iterator i = this.childs.iterator();
				while (i.hasNext()) {
					item = (CheckItem)i.next();
					item.setUserState(item.getState());
				}

				// 2. set new state for self
				CheckItem[] childs = this.getChildrens();
				int childState = childs[0].getState();
				for (int j=1;j<childs.length;++j) {
					if (childState != childs[j].getState()) {
						childState = ICheckable.STATE_HALFCHECK;
						break;
					}
				}
				this.setState(this, childState);
				
				// 3. notify parent
				if (parent!=null) parent.toggle(element);
			}
			
		}
		
		private int getNextState() {
			if (this.getState() == ICheckable.STATE_UNCHECK) {
				// from uncheck to half-check or check
				if (this.hasChildren()) {
					boolean skipHalf = Boolean.TRUE.booleanValue();
					if (this.hasChildren()) {
						CheckItem[] childs = this.getChildrens();
						int childUserState = childs[0].getUserState();
						for (int i=1;i<childs.length;++i) {
							if (childs[i].getUserState() == ICheckable.STATE_HALFCHECK) {
								skipHalf = Boolean.FALSE.booleanValue();
								break;
							}
							if (childUserState != childs[i].getUserState()) {
								skipHalf = Boolean.FALSE.booleanValue();
								break;
							}
						}
					}
					if (skipHalf) {
						return ICheckable.STATE_CHECK;
					} else {
						return ICheckable.STATE_HALFCHECK;
					}
				} else {
					return ICheckable.STATE_CHECK;
				}
			} else if (this.getState() == ICheckable.STATE_HALFCHECK) {
				// from half-check to check
				return ICheckable.STATE_CHECK;
			} else {
				// from check to unckeck
				return ICheckable.STATE_UNCHECK;
			}
		}
		
		public boolean hasChildUserState() {
			Iterator i = this.childs.iterator();
			while (i.hasNext()) {
				if (((CheckItem)i.next()).hasChildUserState()) return Boolean.TRUE.booleanValue();
			}
			return (this.state != this.userState);
		}

		public int getUserState() {
			return userState;
		}

		public void setUserState(int userState) {
			this.userState = userState;
/*
			if (!this.hasChildren()) {
				if (parent!=null) {
					parent.setUserState(userState);
				}
			} else {
				// notify about user change same node
				// 1. copy from state to user state
				Iterator i = this.childs.iterator();
				while (i.hasNext()) {
					((CheckItem)i.next()).setUserState(((CheckItem)i.next()).getState());
				}
			}
*/			
		}

		public Object getData() {
			return data;
		}

		public void setData(Object data) {
			this.data = data;
		}
	}
	
	protected class CheckLabelProvider extends LabelProvider {
		public Image getImage(Object element) {
			if (((CheckItem)element).getState() == ICheckable.STATE_UNCHECK) {
				return IMAGE_UNCHECK;
			} else if (((CheckItem)element).getState() == ICheckable.STATE_CHECK) {
				return IMAGE_CHECK;
			} else {
				return IMAGE_HALFCHECK;
			}
		}
		public String getText(Object element) {
			return ((CheckItem)element).getName();
		}
	}
	
	
	public CheckTreeAdapter() {
		tree = new CheckTree();
		tree.addRoot(new CheckItem("Item1", ICheckable.STATE_CHECK));
		tree.addRoot(new CheckItem("Item2", ICheckable.STATE_UNCHECK));
		tree.addRoot(new CheckItem("Item3", ICheckable.STATE_HALFCHECK));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
	 */
	public Object[] getChildren(Object parentElement) {
		return ((CheckItem)parentElement).getChildrens();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
	 */
	public Object getParent(Object element) {
		return ((CheckItem)element).getParent();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
	 */
	public boolean hasChildren(Object element) {
		return ((CheckItem)element).hasChildren();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
	 */
	public Object[] getElements(Object inputElement) {
		return tree.getRoot();
	}

	public void dispose() {
		super.dispose();
		if (tree!=null) tree.dispose();
		if (labelProvider!=null) labelProvider.dispose();
		selectionChangedListener = null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
	 */
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}


	public Object getAdapter(Class adapter) {
		if (adapter == ILabelProvider.class) {
			if (labelProvider==null) labelProvider = new CheckLabelProvider();
			return labelProvider;
		}
		if (adapter == ITreeContentProvider.class) {
			return this;
		}
		//if (adapter == )
		return super.getAdapter(adapter);
	}

}
