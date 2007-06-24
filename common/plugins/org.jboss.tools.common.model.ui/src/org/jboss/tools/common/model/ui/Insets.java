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
package org.jboss.tools.common.model.ui;

public class Insets {
	public int bottom; 
	public int left; 
	public int right ;
	public int top; 

	public Insets(int top, int left, int bottom, int right) {
		this.top = top;
		this.left = left;
		this.bottom = bottom;
		this.right = right;
	}
	
	public boolean equals(Object o) {
		if (o!=null) {
			return (
				(((Insets)o).top==top)&&
				(((Insets)o).left==left)&&
				(((Insets)o).bottom==bottom)&&
				(((Insets)o).right==right)
			);
		}
		return false;
	}
	
	public int hashCode() {
		return toString().hashCode();
	}
	
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append("Insets: ");
		buffer.append(top);
		buffer.append(", ");
		buffer.append(left);
		buffer.append(", ");
		buffer.append(bottom);
		buffer.append(", ");
		buffer.append(right);
		return buffer.toString();
	}

	public Object clone() {
		return new Insets(top, left, bottom, right);	
	}
}
