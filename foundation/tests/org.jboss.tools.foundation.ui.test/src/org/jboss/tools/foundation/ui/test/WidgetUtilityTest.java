/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.foundation.ui.test;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.ui.widget.IWidgetVisitor;
import org.jboss.tools.foundation.ui.widget.WidgetUtility;
import org.junit.Assert;
import org.junit.Test;


public class WidgetUtilityTest{
	@Test
	public void testEnableAllChildren(){
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		Composite c = new Composite(shell, SWT.NONE);
		Text t1 = new Text(c, SWT.NONE);
		Text t2 = new Text(c, SWT.NONE);
		Text t3 = new Text(c, SWT.NONE);
		Assert.assertTrue(t1.getEnabled());
		Assert.assertTrue(t2.getEnabled());
		Assert.assertTrue(t3.getEnabled());
		new WidgetUtility().enableAllChildren(false, c);
		Assert.assertFalse(t1.getEnabled());
		Assert.assertFalse(t2.getEnabled());
		Assert.assertFalse(t3.getEnabled());
		new WidgetUtility().enableAllChildren(true, c);
		Assert.assertTrue(t1.getEnabled());
		Assert.assertTrue(t2.getEnabled());
		Assert.assertTrue(t3.getEnabled());
	}
	@Test
	public void testDoForAll(){
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		Composite c = new Composite(shell, SWT.NONE);
		Text t1 = new Text(c, SWT.NONE);
		Text t2 = new Text(c, SWT.NONE);
		Text t3 = new Text(c, SWT.NONE);
		Assert.assertTrue(t1.getEditable());
		Assert.assertTrue(t2.getEditable());
		Assert.assertTrue(t3.getEditable());
		new WidgetUtility().doForAllChildren(new IWidgetVisitor() {
			public void visit(Control control) {
				if( control instanceof Text) {
					((Text)control).setEditable(false);
				}
			}
		}, c);
		Assert.assertFalse(t1.getEditable());
		Assert.assertFalse(t2.getEditable());
		Assert.assertFalse(t3.getEditable());
		new WidgetUtility().doForAllChildren(new IWidgetVisitor() {
			public void visit(Control control) {
				if( control instanceof Text) {
					((Text)control).setEditable(true);
				}
			}
		}, c);
		Assert.assertTrue(t1.getEditable());
		Assert.assertTrue(t2.getEditable());
		Assert.assertTrue(t3.getEditable());
	}
}
