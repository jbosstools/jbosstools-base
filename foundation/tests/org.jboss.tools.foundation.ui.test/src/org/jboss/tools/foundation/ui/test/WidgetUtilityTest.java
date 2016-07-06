/******************************************************************************* 
 * Copyright (c) 2016 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.foundation.ui.test;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.ui.widget.IWidgetVisitor;
import org.jboss.tools.foundation.ui.widget.WidgetVisitorUtility;
import org.junit.Assert;
import org.junit.Test;


public class WidgetUtilityTest{
	@Test
	public void testEnableAllChildren(){
		Shell shell = new Shell();
		Composite c = new Composite(shell, SWT.NONE);
		Text t1 = new Text(c, SWT.NONE);
		Text t2 = new Text(c, SWT.NONE);
		Text t3 = new Text(c, SWT.NONE);
		Assert.assertTrue(t1.getEnabled());
		Assert.assertTrue(t2.getEnabled());
		Assert.assertTrue(t3.getEnabled());
		Assert.assertTrue(c.getEnabled());
		new WidgetVisitorUtility().setEnablementRecursive(c, false);
		Assert.assertFalse(t1.getEnabled());
		Assert.assertFalse(t2.getEnabled());
		Assert.assertFalse(t3.getEnabled());
		Assert.assertFalse(c.getEnabled());
		new WidgetVisitorUtility().setEnablementRecursive(c, true);
		Assert.assertTrue(t1.getEnabled());
		Assert.assertTrue(t2.getEnabled());
		Assert.assertTrue(t3.getEnabled());
		Assert.assertTrue(c.getEnabled());
	}
	
	@Test
	public void testDoForAllChildren(){
		Shell shell = new Shell();
		Composite c = new Composite(shell, SWT.NONE);
		Text t1 = new Text(c, SWT.NONE);
		Text t2 = new Text(c, SWT.NONE);
		Text t3 = new Text(c, SWT.NONE);
		Assert.assertTrue(t1.getEditable());
		Assert.assertTrue(t2.getEditable());
		Assert.assertTrue(t3.getEditable());
		new WidgetVisitorUtility(false).accept(c, new IWidgetVisitor() {
			public boolean visit(Control control) {
				if( control instanceof Text) {
					((Text)control).setEditable(false);
				}
				return true;
			}
		});
		Assert.assertFalse(t1.getEditable());
		Assert.assertFalse(t2.getEditable());
		Assert.assertFalse(t3.getEditable());
		new WidgetVisitorUtility(false).accept(c, new IWidgetVisitor() {
			public boolean visit(Control control) {
				if( control instanceof Text) {
					((Text)control).setEditable(true);
				}
				return true;
			}
		});
		Assert.assertTrue(t1.getEditable());
		Assert.assertTrue(t2.getEditable());
		Assert.assertTrue(t3.getEditable());
	}
	

	@Test
	public void testAccepts(){
		Shell shell = new Shell();
		Composite c = new Composite(shell, SWT.NONE);
		Text t1 = new Text(c, SWT.NONE);
		Text t2 = new Text(c, SWT.NONE);
		Text t3 = new Text(c, SWT.NONE);
		Assert.assertTrue(t1.getEnabled());
		Assert.assertTrue(t2.getEnabled());
		Assert.assertTrue(t3.getEnabled());
		Assert.assertTrue(c.getEnabled());
		new WidgetVisitorUtility().accept(c, new IWidgetVisitor() {
			public boolean visit(Control control) {
				control.setEnabled(false);
				return true;
			}
		});
		Assert.assertFalse(t1.getEnabled());
		Assert.assertFalse(t2.getEnabled());
		Assert.assertFalse(t3.getEnabled());
		Assert.assertFalse(c.getEnabled());
		new WidgetVisitorUtility().accept(c, new IWidgetVisitor() {
			public boolean visit(Control control) {
				control.setEnabled(true);
				return true;
			}
		});
		Assert.assertTrue(t1.getEnabled());
		Assert.assertTrue(t2.getEnabled());
		Assert.assertTrue(t3.getEnabled());
		Assert.assertTrue(c.getEnabled());
	}
	

	@Test
	public void testAcceptVisitsExpectedControls(){
		Shell shell = new Shell();
		Composite c = new Composite(shell, SWT.NONE);
		Composite child = new Composite(c, SWT.NONE);
		Composite child2 = new Composite(c, SWT.NONE);
		Text t1 = new Text(child2, SWT.NONE);
		Text t2 = new Text(child2, SWT.NONE);
		Text t3 = new Text(child2, SWT.NONE);
		
		final ArrayList<Control> list = new ArrayList<Control>();
		IWidgetVisitor v = new IWidgetVisitor() {
			public boolean visit(Control control) {
				list.add(control);
				return true;
			}
		};
		
		new WidgetVisitorUtility(false).accept(c,v);
		Assert.assertTrue(list.contains(t1));
		Assert.assertTrue(list.contains(t2));
		Assert.assertTrue(list.contains(t3));
		Assert.assertTrue(list.contains(child));
		Assert.assertTrue(list.contains(child2));
		Assert.assertFalse(list.contains(c));

		new WidgetVisitorUtility(true).accept(c,v);
		Assert.assertTrue(list.contains(t1));
		Assert.assertTrue(list.contains(t2));
		Assert.assertTrue(list.contains(t3));
		Assert.assertTrue(list.contains(child));
		Assert.assertTrue(list.contains(child2));
		Assert.assertTrue(list.contains(c));
	}
}
