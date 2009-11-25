 /*******************************************************************************
  * Copyright (c) 2007 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributors:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.ui.widget.editor;

import java.beans.PropertyChangeListener;
import java.util.Collections;
import java.util.List;

/**
 * 
 * @author Viacheslav Kabanovich
 */
public class TaggedRadioFieldEditor extends CompositeEditor implements ITaggedFieldEditor, PropertyChangeListener{

	List values = null;
	RadioFieldEditor radios = null;
	
	public TaggedRadioFieldEditor(String name, String label, List<String> labels, List values, 
									Object defaultValue) {
		
		super(name, label, defaultValue==null?"":defaultValue.toString()); //$NON-NLS-1$
		this.values = Collections.unmodifiableList(values);
		radios = new RadioFieldEditor(
						name,label,labels, values,getValue());
		addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
																		radios});
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.ITaggedFieldEditor#getTags()
	 */
	public String[] getTags() {
		return radios.getTags();
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.ITaggedFieldEditor#setTags(java.lang.String[])
	 */
	public void setTags(String[] tags) {
		radios.setTags(tags);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.CompositeEditor#setEditable(boolean)
	 */
	@Override
	public void setEditable(boolean ediatble) {
		radios.setEditable(ediatble);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.CompositeEditor#isEditable()
	 */
	@Override
	public boolean isEditable() {
		return radios.isEditable();
	}
}
