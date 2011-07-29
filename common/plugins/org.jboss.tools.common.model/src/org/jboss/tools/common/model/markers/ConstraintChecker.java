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
package org.jboss.tools.common.model.markers;

import org.jboss.tools.common.model.impl.XModelObjectImpl;
import org.jboss.tools.common.model.markers.ResourceMarkers;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.PositionHolder;

public class ConstraintChecker {
	protected XModelObject object;
	protected ResourceProblems p;
	
	public ConstraintChecker(XModelObject object) {
		setModelObject(object);
	}

	public void setModelObject(XModelObject object) {
		this.object = object; 
		p = new ResourceProblems(object);
	}
	
	public void check() {
		if(object == null || !object.isActive()) return;
		boolean empty = p.problems.isEmpty();
		p.problems.clear();
		check(object);
		if(!empty || !p.problems.isEmpty()) {
			addMarkers();
		}
	}
	
	private void check(XModelObject o) {
		XAttribute[] as = o.getModelEntity().getAttributes();
		String idAttr = null;
		for (int i = 0; i < as.length; i++) {
			String error = as[i].getConstraint().getError(o.getAttributeValue(as[i].getName()));
			if(error != null) addProblem(o, as[i].getName(), "Value " + error);
			if("true".equals(as[i].getProperty("id"))) idAttr = as[i].getName();
		}
		XModelObject[] cs = ((XModelObjectImpl)o).getLoadedChildren();
		for (int i = 0; i < cs.length; i++) check(cs[i]);

		String duplicate = o.get(XModelObjectImpl.DUPLICATE);
		if(duplicate != null && duplicate.length() > 0 && idAttr != null) {
			addProblem(o, idAttr, "Value " + o.getAttributeValue(idAttr) + " is not unique.");
		}
	}

	protected void addProblem(XModelObject o, String attr, String msg) {
		if(o == null) return;
		if(msg != null) {
			PositionHolder h = PositionHolder.getPosition(o, attr);
			h.update();
			p.addError(o.getPath(), msg, attr, h);
		} 
	}
	
	protected void addMarkers() {
		ResourceMarkers markers = new ConstraintMarkers(p);
		markers.setModelObject(p.resourceObject);
		markers.update();
	}
}
