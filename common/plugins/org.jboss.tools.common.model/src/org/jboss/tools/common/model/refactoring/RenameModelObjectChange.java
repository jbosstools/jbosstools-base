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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.internal.corext.refactoring.changes.TextChangeCompatibility;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.ReplaceEdit;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.impl.XModelObjectImpl;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.PositionSearcher;

/**
 * @author glory
 * 
 * This change receives an array of objects which reference 
 * to a renamed object, name of attribute by which they 
 * reference to it, and new value. The change implementation
 * presents and performs the change as a text change.
 * 
 * Use restrictions:
 * 1. All objects must belong to the same file object.
 * 2. You should not create to independent changes that 
 *    process the same file object. For instance, if you need 
 *    process two sets of objects from one file with different 
 *    attributes affected, this implementation may not be used. 
 */
public class RenameModelObjectChange extends TextFileChange {
	private XModelObject[] objects;
	private String newName;
	private boolean ok = false;
	private String attributeName;
	
	public static RenameModelObjectChange createChange(XModelObject[] objects, String newName, String attributeName) {
		if(objects == null || objects.length == 0) return null;
		String name = objects[0].getPresentationString();
		IFile f = getFile(objects[0]);
		if(f == null) return null;
		return new RenameModelObjectChange(name, f, objects, newName, attributeName);
	}
	
	private RenameModelObjectChange(String name, IFile file, XModelObject[] objects, String newName, String attributeName) {
		super(name, file);
		this.objects = objects;
		this.newName = newName;
		this.attributeName = attributeName;
		addEdits();
	}

	private void addEdits() {
		PositionSearcher searcher = new PositionSearcher();
		XModelObject o = ((XModelObjectImpl)objects[0]).getResourceAncestor();
		String text = ((FileAnyImpl)o).getAsText();
		for (int i = 0; i < objects.length; i++) {
			searcher.init(text, objects[i], attributeName);
			searcher.execute();
			int bp = searcher.getStartPosition();
			int ep = searcher.getEndPosition();
			ok = false;
			if(bp >= 0 && ep >= ep) {
				ReplaceEdit edit = new ReplaceEdit(bp, ep - bp, newName);
				TextChangeCompatibility.addTextEdit(this, "Update field reference", edit);
				ok = true;
			}
		}
	}
	
	protected static IFile getFile(XModelObject object) {
		XModelObject o = ((XModelObjectImpl)object).getResourceAncestor();
		return o == null ? null : (IFile)EclipseResourceUtil.getResource(o);
	}

	public Change perform(IProgressMonitor pm) throws CoreException {
		if(ok) {
			return super.perform(pm);
		}
		try {
			for (int i = 0; i < objects.length; i++) {
				objects[i].getModel().changeObjectAttribute(objects[i], attributeName, newName);
			}
		} catch (Exception e) {
			ModelPlugin.log(e);
		}		
		return null;
	}

	public Object getModifiedElement() {
		return null;
	}
	
	public String getName() {
		return "Edit " + attributeName;
	}

}
