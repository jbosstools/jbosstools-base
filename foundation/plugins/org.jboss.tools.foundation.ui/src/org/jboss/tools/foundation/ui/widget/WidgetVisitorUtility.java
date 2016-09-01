/*******************************************************************************
  * Copyright (c) 2016 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.foundation.ui.widget;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * A utility for all tasks that may require logic for visiting controls,
 * composites, or other widgets. 
 */
public class WidgetVisitorUtility {
	
	public static final int DEPTH_FIRST = 1;
	public static final int BREADTH_FIRST = 2;
	
	
	private int traversal;
	private boolean visitRoot;
	
	/**
	 * Uses a breadth-first traversal that does visit the root element
	 */
	public WidgetVisitorUtility() {
		this(BREADTH_FIRST);
	}
	
	/**
	 * Uses an arbitrary traversal that does visit the root element
	 * 
	 * @param traversal One of DEPTH_FIRST or BREADTH_FIRST
	 */
	public WidgetVisitorUtility(int traversal) {
		this(traversal, true);
	}

	/**
	 * Uses a breadth-first traversal that may or may not visit the root element
	 * 
	 * @param visitRoot Whether to visit the root element
	 */
	public WidgetVisitorUtility(boolean visitRoot) {
		this(BREADTH_FIRST, visitRoot);
	}

	/**
	 * Create a visitor utility using a traversal type as provided, 
	 * and visiting root element as provided.
	 * 
	 * @param traversal
	 * @param visitRoot
	 */
	public WidgetVisitorUtility(int traversal, boolean visitRoot) {
		this.traversal = traversal;
		this.visitRoot = visitRoot;
	}


	/**
	 * Optionally enable or disable all children (recursive) of this composite
	 * @param enabled
	 * @param composite
	 */
	public void setEnablementRecursive(final Composite composite, final boolean enabled) {
		accept(composite, new IWidgetVisitor() {
			@Override
			public boolean visit(Control control) {
				if(!control.isDisposed()) {
					control.setEnabled(enabled);
				}
				return true;
			}
		});
	}
	public void setEnablementRecursive(final Composite composite, final boolean enabled, Control[] ignored) {
		if( ignored == null ) {
			setEnablementRecursive(composite, enabled);
			return;
		}
		
		
		boolean[] status = new boolean[ignored.length];
		if( status != null ) {
			for( int i = 0; i < status.length; i++ ) {
				status[i] = ignored[i].getEnabled();
			}
		}
		setEnablementRecursive(composite, enabled);
		for( int i = 0; i < ignored.length; i++ ) {
			ignored[i].setEnabled(status[i]);
			if( status[i] ) {  
				// if this 'ignored' should be enabled, set all parents as enabled too
				Composite c = ignored[i].getParent();
				while( c != null ) {
					if( !c.isDisposed()) {
						c.setEnabled(true);
						c = c.getParent();
					}
				}
			}
		}
	}

	/**
	 * Accepts the given visitor.
	 * 
	 * The visitor's <code>visit</code> method is called on the given composite first.
	 *  
	 * The list of children for the given composite will then be visited.
	 * If the <code>visit</code> method returns true for a given child
	 * widget, and that widget is also a composite, the <code>accept</code>
	 * method will be called on all children of that composite as well.   
	 * 
	 * This is a shortcut accept(visitor, composite, false)
	 * 
	 * @param composite   The composite to visit
	 * @param visitor     The visitor
	 */
	public void accept(Composite composite, IWidgetVisitor visitor) {
		accept(composite, visitor, (traversal == DEPTH_FIRST), visitRoot);
	}
	
	
	/**
	 * Accepts the given visitor.
	 * 
	 * The visitor's <code>visit</code> method is called on the given composite first, 
	 * or last, depending on the value of <code>depthFirst</code>. This will not occur
	 * if <code>visitRoot</code> is set to <code>false</code>
	 *  
	 * The list of children for the given composite will then be visited.
	 * If the <code>visit</code> method returns true for a given child
	 * widget, and that widget is also a composite, the <code>accept</code>
	 * method will be called on all children of that composite as well.   
	 * 
	 * @param composite   The composite to visit
	 * @param visitor     The visitor
	 * @param depthFirst  Whether to perform depth first or not
	 * @param visitRoot Whether to visit the parent or to proceed directly to the children
	 */
	private void accept(Composite composite, IWidgetVisitor visitor, boolean depthFirst, boolean visitRoot) {

		if (composite == null || composite.isDisposed()) {
			// Do not visit children if parent is disposed or null, 
			// but do continue with siblings
			return;
		}
		
		Control children[] = composite.getChildren();
		boolean traverseChildren = true;

		if (!depthFirst && visitRoot)
			traverseChildren = visitor.visit(composite);

		if (traverseChildren) {
			for (int i = 0; i < children.length; i++) {
				if (children[i] instanceof Composite) {
					accept(((Composite)children[i]), visitor, depthFirst, true);
				} else {
					visitor.visit(children[i]);
				}
			}
		}

		if (depthFirst && visitRoot)
			visitor.visit(composite);
	}
	
}
