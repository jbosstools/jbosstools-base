package org.jboss.tools.runtime.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;

public class RuntimeCheckboxTreeViewer extends CheckboxTreeViewer {
	public RuntimeCheckboxTreeViewer(Composite parent, final Set<RuntimePath> runtimePaths2, int heightHint) {
		this(parent, runtimePaths2.toArray(new RuntimePath[runtimePaths2.size()]), heightHint);
	}
	
	public RuntimeCheckboxTreeViewer(Composite parent, final RuntimePath[] runtimePaths2, int heightHint) {
		super(parent, SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
		
		GridData gd;
		Tree tree = getTree();
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		GC gc = new GC( parent);
		FontMetrics fontMetrics = gc.getFontMetrics( );
		gc.dispose( );
		gd.minimumHeight = Dialog.convertHeightInCharsToPixels(fontMetrics, heightHint);
		tree.setLayoutData(gd);
		tree.setHeaderVisible(true);
		tree.setLinesVisible(true);

		String[] columnNames = new String[] { "Name", "Version", "Type", "Location"};
		int[] columnWidths = new int[] {300, 100, 50, 200};
		
		for (int i = 0; i < columnNames.length; i++) {
			TreeViewerColumn tc = new TreeViewerColumn(this, SWT.NONE);
			tc.getColumn().setText(columnNames[i]);
			tc.getColumn().setWidth(columnWidths[i]);
		}

		setLabelProvider(new RuntimeLabelProvider());
		List<RuntimeDefinition> runtimeDefinitions = new ArrayList<RuntimeDefinition>();
		for (RuntimePath runtimePath:runtimePaths2) {
			runtimeDefinitions.addAll(Arrays.asList(runtimePath.getRuntimeDefinitions()));
		}
		setContentProvider(new RuntimeContentProvider(runtimeDefinitions));
		setInput(runtimeDefinitions);
		for (RuntimeDefinition definition:runtimeDefinitions) {
			setChecked(definition, definition.isEnabled());
		}
	}
	
	// Refresh your input given the following RuntimePath[]
	public void updateInput(RuntimePath[] runtimePaths) {
		setInput(null);
		List<RuntimeDefinition> runtimeDefinitions = new ArrayList<RuntimeDefinition>();
		for (RuntimePath runtimePath : runtimePaths) {
			runtimeDefinitions.addAll(Arrays.asList(runtimePath.getRuntimeDefinitions()));
		}
		setInput(runtimeDefinitions);
		for (RuntimeDefinition runtimeDefinition : runtimeDefinitions) {
			setChecked(runtimeDefinition,runtimeDefinition.isEnabled());
		}
	}
}
