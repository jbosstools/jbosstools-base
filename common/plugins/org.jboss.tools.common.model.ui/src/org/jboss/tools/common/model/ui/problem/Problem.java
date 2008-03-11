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
package org.jboss.tools.common.model.ui.problem;

import java.util.ArrayList;

import org.eclipse.jface.action.IAction;

/**
 * @author Aleksey
 */
public class Problem {
	
	public static final String ERROR = "Problem.Error";

	public static final int NONE = -1;

	private String type;
	private String message;
	private String title;
	private String location;
	private int line = NONE;
	private int column = NONE;
	private int position = NONE;
	private ArrayList<IAction> actions = new ArrayList<IAction>();

	private Problem() {}
	
	public Problem(String message) {
		this.message = message;
		this.type = ERROR;
	}
	
	public Problem(String message, int position, int line, int column) {
		this.message = message;
		this.position = position;
		this.line = line;
		this.column = column;
		this.type = ERROR;
	}
	
	public void addActions(IAction[] actions) {
		for (int i = 0; i < actions.length; ++i) {
			this.actions.add(actions[i]);
		}
	}
	
	public void addAction(ProblemDialogAction action) {
		if (action!=null) actions.add(action);
	}
	
	public int getColumn() {
		return column;
	}
	
	public int getPosition() {
		return position;
	}

	public int getLine() {
		return line;
	}

	public String getLocation() {
		return location;
	}

	public String getMessage() {
		return message;
	}

	public String getTitle() {
		return title;
	}

	public void setColumn(int i) {
		column = i;
	}

	public void setLine(int i) {
		line = i;
	}

	public void setLocation(String string) {
		location = string;
	}

	public void setMessage(String string) {
		message = string;
	}

	public void setTitle(String string) {
		title = string;
	}

	public ArrayList getActions() {
		return actions;
	}

	public String getType() {
		return type;
	}

	public void setType(String string) {
		type = string;
	}
	
	public int hashCode() {
		return line + ((message == null) ? NONE : message.hashCode())
		 + ((location == null) ? NONE : location.hashCode());
	}
	
	public boolean equals(Object object) {
		if(!(object instanceof Problem)) return false;
		Problem other = (Problem)object;
		return other.line == line 
///			&& other.column == column 
		    && equalStrings(other.message, message)
///			&& equalStrings(other.type, type)
			&& equalStrings(other.location, location);
	}
	
	private boolean equalStrings(String s1, String s2) {
		return (s1 == null && s2 == null) || (s1 != null && s1.equals(s2)); 
	}
	
	public String toString() {
		return "type=" + type + " location=" + location + " message=" + message + " line=" + line + " column=" + column;
	}

}
