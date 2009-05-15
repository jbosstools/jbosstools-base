 /*******************************************************************************
  * Copyright (c) 2007 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.model.project.ext.event;

import java.util.ArrayList;
import java.util.List;

/**
 * This object collects changes in target that should be fired to listeners.
 * 
 * @author Viacheslav Kabanovich
 */
public class Change {
	Object target;
	String property;
	Object oldValue;
	Object newValue;
	List<Change> children;
	
	/**
	 * Constructs object with initial values
	 * 
	 * @param target
	 * @param property
	 *            name of property changed or null, if change is adding/removing
	 *            a child.
	 * @param oldValue
	 *            old value; if null and property = null, then child (newValue)
	 *            is added
	 * @param newValue
	 *            new value; if null and property = null, then child (oldValue)
	 *            is removed
	 */
	public Change(Object target, String property, Object oldValue, Object newValue) {
		this.target = target;
		this.property = property;
		this.oldValue = oldValue;
		this.newValue = newValue;
	}
	
	public Object getTarget() {
		return target;
	}
	
	public String getProperty() {
		return property;
	}
	
	public Object getOldValue() {
		return oldValue;
	}

	public Object getNewValue() {
		return newValue;
	}
	
	public void addChildren(List<Change> children) {
		if(this.children == null) {
			this.children = children;
		} else if(children != null) {
			this.children.addAll(children);
		}
	}
	
	/**
	 * Returns true if this object defines no actual change in seam model.
	 * @return
	 */
	public boolean isEmpty() {
		return oldValue == null && newValue == null && !isChildrenAffected();
	}
	
	/**
	 * Returns true if this change includes sub-changes.
	 * @return
	 */
	public boolean isChildrenAffected() {
		return children != null && !children.isEmpty();
	}
	
	/**
	 * Returns list of all changes
	 * @return
	 */
	public List<Change> getChildren() {
		return children;
	}
	
	/**
	 * Invokes visitor for this change, and if visit returns true, iterates over 
	 * child changes.
	 * @param visitor
	 */
	public void visit(IChangeVisitor visitor) {
		if(!visitor.visit(this)) return;
		if(children != null) {
			for (Change c: children) {
				c.visit(visitor);
			}
		}
	}

	/**
	 * Utility method to attach a single change to the list. If list is not provided,
	 * new list is created, otherwise the provided list is returned.
	 * @param changes
	 * @param change
	 * @return
	 */
	public static List<Change> addChange(List<Change> changes, Change change) {
		if(change == null || change.isEmpty()) return changes;
		if(changes == null) changes = new ArrayList<Change>();
		changes.add(change);
		return changes;
	}
	
}
