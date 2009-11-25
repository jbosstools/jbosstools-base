package org.jboss.tools.common.ui.widget.field;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Link;
import org.jboss.tools.common.ui.widget.editor.ButtonFieldEditor.ButtonPressedAction;

public class LinkField extends BaseField {

	Link link;
	
	/**
	 * 
	 */
	@Override
	public Control getControl() {
		return link;
	}


	public LinkField(Composite composite, ButtonPressedAction listener) {
		link = new Link(composite,SWT.NONE);
		link.setText(listener.getText());
		link.addSelectionListener(listener);
	}

}
