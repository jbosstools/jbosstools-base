/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.reporting;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import org.jboss.tools.usage.util.BundleUtils.IBundleEntryFilter;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class JBossBundleGroups implements IBundleEntryFilter {

	public enum BundleGroup {
		ARCHIVES("org.jboss.ide.eclipse.archives.core",
				"org.jboss.ide.eclipse.archives.jdt.integration",
				"org.jboss.ide.eclipse.archives.ui"
				)
		, AS("org.jboss.ide.eclipse.archives.webtools",
				"org.jboss.ide.eclipse.as.classpath.core",
				"org.jboss.ide.eclipse.as.classpath.ui",
				"org.jboss.ide.eclipse.as.core",
				"org.jboss.ide.eclipse.as.doc.user",
				"org.jboss.ide.eclipse.as.ssh",
				"org.jboss.ide.eclipse.as.ui",
				"org.jboss.ide.eclipse.as.ui.mbeans",
				"org.jboss.ide.eclipse.as.wtp.core",
				"org.jboss.ide.eclipse.as.wtp.ui")
		, BIRT("org.jboss.tools.birt.core",
				"org.jboss.tools.birt.oda",
				"org.jboss.tools.birt.oda.ui")
		, BPEL("org.eclipse.bpel.apache.ode.deploy.model",
				"org.eclipse.bpel.apache.ode.deploy.ui",
				"org.eclipse.bpel.common.model",
				"org.eclipse.bpel.common.ui",
				"org.eclipse.bpel.model",
				"org.eclipse.bpel.ui",
				"org.eclipse.bpel.validator",
				"org.eclipse.bpel.wsil.model",
				"org.eclipse.bpel.xpath10",
				"org.jboss.tools.bpel.cheatsheet",
				"org.jboss.tools.bpel.runtimes")
		, CDI("org.jboss.tools.cdi.core",
				"org.jboss.tools.cdi.text.ext",
				"org.jboss.tools.cdi.ui",
				"org.jboss.tools.cdi.xml",
				"org.jboss.tools.cdi.xml.ui")
		, DELTACLOUD("org.jboss.tools.deltacloud.core",
				"org.jboss.tools.deltacloud.ui")
		, DROOLS("org.drools.eclipse",
				"org.drools.eclipse.task",
				"org.eclipse.webdav",
				"org.guvnor.tools",
				"org.jboss.tools.flow.ruleflow")
		, ESB("org.jboss.tools.esb.core",
				"org.jboss.tools.esb.project.core",
				"org.jboss.tools.esb.project.ui",
				"org.jboss.tools.esb.ui",
				"org.jboss.tools.esb.validator")
		, FLOW("org.jboss.tools.flow.common")
		, FREEMARKER("org.jboss.ide.eclipse.freemarker")
		, GWT("org.jboss.tools.gwt.core",
				"org.jboss.tools.gwt.ui")
		, HIBERNATETOOLS("org.hibernate.eclipse",
				"org.hibernate.eclipse.console",
				"org.hibernate.eclipse.help",
				"org.hibernate.eclipse.jdt.apt.ui",
				"org.hibernate.eclipse.jdt.ui",
				"org.hibernate.eclipse.mapper",
				"org.jboss.tools.hibernate.jpt.core",
				"org.jboss.tools.hibernate.jpt.ui",
				"org.jboss.tools.hibernate.ui",
				"org.jboss.tools.hibernate.xml",
				"org.jboss.tools.hibernate.xml.ui")
		, JBPM("org.jboss.tools.flow.jpdl4",
				"org.jboss.tools.flow.jpdl4.multipage",
				"org.jboss.tools.jbpm.common",
				"org.jboss.tools.jbpm.convert",
				"org.jbpm.gd.jpdl")
		, JMX("org.jboss.tools.jmx.core",
				"org.jboss.tools.jmx.ui")
		, JSF("org.jboss.tools.jsf",
				"org.jboss.tools.jsf.doc.user",
				"org.jboss.tools.jsf.text.ext",
				"org.jboss.tools.jsf.text.ext.facelets",
				"org.jboss.tools.jsf.text.ext.richfaces",
				"org.jboss.tools.jsf.ui",
				"org.jboss.tools.jsf.verification",
				"org.jboss.tools.jsf.vpe.ajax4jsf",
				"org.jboss.tools.jsf.vpe.facelets",
				"org.jboss.tools.jsf.vpe.jbpm",
				"org.jboss.tools.jsf.vpe.jsf",
				"org.jboss.tools.jsf.vpe.jstl",
				"org.jboss.tools.jsf.vpe.myfaces",
				"org.jboss.tools.jsf.vpe.richfaces",
				"org.jboss.tools.jsf.vpe.seam")
		, JST("org.jboss.tools.jst.css",
				"org.jboss.tools.jst.firstrun",
				"org.jboss.tools.jst.jsp",
				"org.jboss.tools.jst.text.ext",
				"org.jboss.tools.jst.web",
				"org.jboss.tools.jst.web.kb",
				"org.jboss.tools.jst.web.tiles",
				"org.jboss.tools.jst.web.tiles.ui",
				"org.jboss.tools.jst.web.ui",
				"org.jboss.tools.jst.web.verification")
		, LABS("org.jboss.tools.labs.pde.sourceprovider")
		, MAVEN("org.jboss.tools.maven.cdi",
				"org.jboss.tools.maven.core",
				"org.jboss.tools.maven.hibernate",
				"org.jboss.tools.maven.jsf",
				"org.jboss.tools.maven.portlet",
				"org.jboss.tools.maven.seam",
				"org.jboss.tools.maven.ui")
		, MODESHAPE("org.jboss.tools.modeshape.rest")
		, PORTLET("org.jboss.tools.portlet.core",
				"org.jboss.tools.portlet.ui")
		, PROFILER("org.jboss.tools.profiler.ui")
		, RUNTIME("org.jboss.tools.runtime")
		, SEAM("org.jboss.tools.seam.core",
				"org.jboss.tools.seam.doc.user",
				"org.jboss.tools.seam.pages.xml",
				"org.jboss.tools.seam.text.ext",
				"org.jboss.tools.seam.ui",
				"org.jboss.tools.seam.ui.pages",
				"org.jboss.tools.seam.xml",
				"org.jboss.tools.seam.xml.ui")
		, SMOOKS("org.jboss.tools.smooks.core",
				"org.jboss.tools.smooks.runtime",
				"org.jboss.tools.smooks.templating",
				"org.jboss.tools.smooks.ui")
		, STRUTS("org.jboss.tools.struts",
				"org.jboss.tools.struts.text.ext",
				"org.jboss.tools.struts.ui",
				"org.jboss.tools.struts.validator.ui",
				"org.jboss.tools.struts.verification",
				"org.jboss.tools.struts.vpe.struts")
		, TPTP("org.jboss.tools.eclipse.as.tptp")
		, VPE("org.jboss.tools.vpe",
				"org.jboss.tools.vpe.docbook",
				"org.jboss.tools.vpe.html",
				"org.jboss.tools.vpe.jsp",
				"org.jboss.tools.vpe.resref",
				"org.jboss.tools.vpe.spring",
				"org.jboss.tools.vpe.ui.palette",
				"org.jboss.tools.vpe.xulrunner",
				"org.jboss.tools.xulrunner",
				"org.jboss.tools.xulrunner.initializer")
		, WORKINGSET("org.jboss.tools.workingset.core",
				"org.jboss.tools.workingset.ui")
		, WS("rg.jboss.tools.ws.core",
				"org.jboss.tools.ws.creation.core",
				"org.jboss.tools.ws.creation.ui",
				"org.jboss.tools.ws.ui")
		, XULRUNNER("org.mozilla.xpcom",
				"org.mozilla.xulrunner.carbon.macosx",
				"org.mozilla.xulrunner.cocoa.macosx",
				"org.mozilla.xulrunner.gtk.linux.x86",
				"org.mozilla.xulrunner.gtk.linux.x86_64",
				"org.mozilla.xulrunner.win32.win32.x86");

		private String[] bundleNames;

		BundleGroup(String... bundleNames) {
			this.bundleNames = bundleNames;
		}

		/**
		 * Returns <tt>true</tt> if the given bundle name is a member of this
		 * bundle group.
		 * 
		 * @param bundleName
		 *            the bundle name to check whether it's a member of this
		 *            group of bundles.
		 * @return <tt>true</tt>, if the given bundle
		 */
		public boolean isMember(String bundleName) {
			boolean isMember = false;
			for (String name : bundleNames) {
				if (name.equals(bundleName)) {
					return true;
				}
			}
			return isMember;
		}

		public void addGroupIdTo(Collection<String> bundleGroupIds) {
			bundleGroupIds.add(name());
		}
	}

	private Set<String> jbossBundleGroups = new TreeSet<String>();

	/**
	 * Collects the bundle groups the bundles it gets belong
	 * to. Always returns <tt>true</tt> (does match) while collecting
	 */
	public boolean matches(Bundle bundle) {
		String bundleName = bundle.getSymbolicName();
		for (BundleGroup bundleGroup : BundleGroup.values()) {
			if (bundleGroup.isMember(bundleName)) {
				jbossBundleGroups.add(bundleGroup.name());
				break;
			}
		}
		return true;
	}

	public Collection<String> getBundleGroupIds() {
		return jbossBundleGroups;
	}
}