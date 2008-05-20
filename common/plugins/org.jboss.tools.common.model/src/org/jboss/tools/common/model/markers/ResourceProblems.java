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

import java.util.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.PositionHolder;

public class ResourceProblems {
	XModelObject resourceObject;
	ArrayList<ResourceProblem> problems = new ArrayList<ResourceProblem>();
	
	public ResourceProblems(XModelObject resourceObject) {
		this.resourceObject = resourceObject;		
	}

	public void addError(String path, String msg, String attr, PositionHolder h) {
		ResourceProblem p = new ResourceProblem();
		p.path = path;
		p.msg = msg;
		p.attr = attr;
		p.line = h.getLine();
		p.start = h.getStart();
		p.end = h.getEnd();
		problems.add(p);
	}
	
	public void addError(String path, String msg, String attr, int line) {
		ResourceProblem p = new ResourceProblem();
		p.path = path;
		p.msg = msg;
		p.attr = attr;
		p.line = line;
		problems.add(p);
	}
	
	ResourceProblem getProblem(int i) {
		return (ResourceProblem)problems.get(i);
	}
	
	public String[] getMessages() {
		String[] s = new String[problems.size()];
		for (int i = 0; i < s.length; i++) {
			s[i] = getProblem(i).msg;
		}
		return s;
	}
	
	public String getPath(int i) {
		return getProblem(i).path;
	}
	
	public String getAttribute(int i) {
		return getProblem(i).attr;
	}
	
	public XModelObject getResourceObject() {
		return resourceObject;
	}
	
	public int getLine(int i) {
		return getProblem(i).line;
	}

	public int getStart(int i) {
		return getProblem(i).start;
	}

	public int getEnd(int i) {
		return getProblem(i).end;
	}

}
