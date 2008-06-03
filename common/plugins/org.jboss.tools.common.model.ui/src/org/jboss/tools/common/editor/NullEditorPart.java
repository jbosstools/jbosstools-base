package org.jboss.tools.common.editor;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.part.EditorPart;

public class NullEditorPart extends EditorPart {
	
	public NullEditorPart() {}

	public void doSave(IProgressMonitor monitor) {
	}

	public void doSaveAs() {
	}

	public void createPartControl(Composite parent) {
		Composite c = new Composite(parent, SWT.NONE);
		c.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
		c.setLayout(new GridLayout());
		c.setLayoutData(new GridData(GridData.FILL_BOTH));
		Label label = new Label(c, SWT.NONE);
		label.setText("Resource has been externally removed.");
		label.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));		
	}

	public void init(IEditorSite site, IEditorInput input) {
		setSite(site);
		setInput(input);
		setPartName(input.getName());
	}

	public boolean isDirty() {
		return false;
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public void setFocus() {
	}

}
