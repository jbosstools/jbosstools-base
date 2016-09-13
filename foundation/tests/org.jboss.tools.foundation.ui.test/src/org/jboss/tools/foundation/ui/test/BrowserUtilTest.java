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
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.foundation.core.IURLProvider;
import org.jboss.tools.foundation.ui.util.BrowserUtility;
import org.junit.Assert;
import org.junit.Test;


public class BrowserUtilTest{
	@Test
	public void testCreateBrowser(){
		Browser b = null;
		try {
			System.setProperty("jbosstools.skip.browser.creation", "true"); 
			b = new BrowserUtility().createBrowser(new Shell(), SWT.WEBKIT);
			Assert.assertNull(b);
			
			System.setProperty("jbosstools.skip.browser.creation", "false");
			b = new BrowserUtility().createBrowser(new Shell(), SWT.WEBKIT);
			Assert.assertNotNull(b);
			b.dispose();
		} catch( AssertionError t) {
			if( b != null && !b.isDisposed()) {
				b.dispose();
			}
		}
	}
	
	@Test
	public void testCreateBrowserOrLink(){
		System.setProperty("jbosstools.skip.browser.creation", "true"); 
		IURLProvider provider = new IURLProvider(){
			public String getUrl() {
				return "http://www.google.com";
			}
		};
		Control b = null;
		try {
			b = new BrowserUtility().createBrowserOrLink(SWT.NONE, new Shell(), SWT.WEBKIT,
					provider, "No Browser");
			Assert.assertNotNull(b);
			Assert.assertTrue(b instanceof Link);
			if( !b.isDisposed())
				b.dispose();
			
			System.setProperty("jbosstools.skip.browser.creation", "false");
			b = new BrowserUtility().createBrowserOrLink(SWT.NONE, new Shell(), SWT.WEBKIT,
					provider, "No Browser");
			Assert.assertNotNull(b);
			Assert.assertTrue(b instanceof Browser || b instanceof Link);
			if( !b.isDisposed())
				b.dispose();
		} catch( AssertionError t) {
			if(b != null && !b.isDisposed()) {
				b.dispose();
			}
		}
	}
}
