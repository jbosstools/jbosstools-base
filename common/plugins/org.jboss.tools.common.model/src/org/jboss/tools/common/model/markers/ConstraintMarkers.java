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

import org.jboss.tools.common.model.markers.ResourceMarkers;

public class ConstraintMarkers extends ResourceMarkers {
	ResourceProblems p;

	public ConstraintMarkers(ResourceProblems p) {
		super(ResourceMarkers.CONSTRAINT_PROBLEM);
		this.p = p;
	}
	
	protected String[] getErrors() {
		String[] rs = p.getMessages();
		for (int i = 0; i < rs.length; i++)
		  rs[i] = getTrueMessage(rs[i]);
		return rs;
	}
		
	protected String getObjectPathForError(int i) {
		return p.getPath(i);
	}
	
	protected String getObjectAttributeForError(int i) {
		return p.getAttribute(i);
	}	

	protected int getLocation(int i) {
		return p.getLine(i);
	}

	protected int getStart(int i) {
		return p.getStart(i);
	}
	
	protected int getEnd(int i) {
		return p.getEnd(i);
	}
	
}
