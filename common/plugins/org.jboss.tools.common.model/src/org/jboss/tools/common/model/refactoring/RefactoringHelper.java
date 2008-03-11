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
package org.jboss.tools.common.model.refactoring;

import java.util.*;
import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.internal.corext.refactoring.changes.TextChangeCompatibility;
import org.eclipse.ltk.core.refactoring.*;
import org.eclipse.text.edits.ReplaceEdit;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class RefactoringHelper {
	
	/**
	 * Builds text file change for a file prisented by fileObject,
	 * based on pairs oldText - newText stored in replacements, 
	 * and adds the new change to parentChange
	 * @param fileObject
	 * @param replacements - pairs oldText - newText
	 * @param parentChange - parent change for builded text file change
	 */	
	public static void addChanges(XModelObject fileObject, Properties replacements, CompositeChange parentChange) {
		if(replacements == null || replacements.size() == 0) return;
		IFile f = (IFile)EclipseResourceUtil.getResource(fileObject);
		if(f == null) return;
		String body = ((FileAnyImpl)fileObject).getAsText();
		if(body == null || replacements.size() == 0) return;
		ReplaceEdit[] edits = RefactoringHelper.getEdits(replacements, body);
		if(edits != null && edits.length > 0) {
			TextFileChange change = new TextFileChange("TextFileChange", f);
			for (int j = 0; j < edits.length; j++) {
				TextChangeCompatibility.addTextEdit(change, "Update Reference", edits[j]);
			}
			parentChange.add(change);
		}
	}
	
	/**
	 * Builds array of replace edits, for given body,
	 * based on pairs oldText - newText stored in replacements
	 * @param replacements - pairs oldText - newText
	 * @param body
	 * @return
	 */
	public static ReplaceEdit[] getEdits(Properties replacements, String body) {
		ArrayList<ReplaceEdit> l = new ArrayList<ReplaceEdit>();
		String[] bs = (String[])replacements.keySet().toArray(new String[0]);
		for (int i = 0; i < bs.length; i++) {
			String b = bs[i];
			int length = b.length();
			String e = replacements.getProperty(b);
			int pos = 0;
			while(true) {
				pos = body.indexOf(b, pos);
				if(pos < 0) break;
				ReplaceEdit edit = new ReplaceEdit(pos, length, e);
				l.add(edit);
				pos += length;
			}
		}
		return l.toArray(new ReplaceEdit[0]);
	}

}
