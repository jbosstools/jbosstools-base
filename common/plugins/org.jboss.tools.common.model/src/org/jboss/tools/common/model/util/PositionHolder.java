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
package org.jboss.tools.common.model.util;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;

public class PositionHolder {
	static String POSITION_HOLDER_ID = "positionHolder";
	
	public static PositionHolder getPosition(XModelObject object, String attribute) {
		String id = (attribute == null) ? POSITION_HOLDER_ID : POSITION_HOLDER_ID + "." + attribute;
		PositionHolder h = (PositionHolder)object.getObject(id);
		if(h == null) {
			h = new PositionHolder(object, attribute);
			object.setObject(id, h);
		}
		return h;
	}
	
	private XModelObject object;
	private String attribute;
	private FileAnyImpl file = null;
	private long fileTimeStamp = -1;
	private int start;
	private int end;
	private int line;
	
	private PositionHolder(XModelObject object, String attribute) {
		this.object = object;
		this.attribute = attribute;
	}
	
	public void update() {
		if(!object.isActive()) {
			if(file == null) return; else file = null;
		} else if(file == null) {
			XModelObject f = object;
			while(f != null && f.getFileType() != XModelObject.FILE) f = f.getParent();
			if(f instanceof FileAnyImpl) file = (FileAnyImpl)f;
		}
		if(file == null) {
			fileTimeStamp = -1;
			start = -1;
			end = -1;
			line = -1;
			return;
		}
		long ts = (file == null) ? -1 : file.getTimeStamp();
		if(ts == fileTimeStamp) return;
		fileTimeStamp = ts;
		String text = file.getAsText();
		PositionSearcher searcher = new PositionSearcher();
		searcher.init(text, object, attribute);
		searcher.execute();
		start = searcher.getStartPosition();
		end = searcher.getEndPosition();
		line = computeLine(text, start);
	}
	
	private int computeLine(String text, int pos) {
		if(pos < 0) return -1;
		int line = 1;
		boolean q = false;
		for (int i = 0; i < text.length() && i < pos; i++) {
			char ch = text.charAt(i);
			if(ch == '\n' || q) line++;
			q = (ch == '\r');
		}
		return line;
	}
	
	public int getLine() {
		return line;
	}
	
	public int getStart() {
		return start;
	}
	
	public int getEnd() {
		return end;
	}

}
