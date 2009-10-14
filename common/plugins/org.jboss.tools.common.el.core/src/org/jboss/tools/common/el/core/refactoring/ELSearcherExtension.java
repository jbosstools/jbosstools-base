package org.jboss.tools.common.el.core.refactoring;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.el.core.Activator;

public class ELSearcherExtension {
	public static String EXTENSION_POINT = "org.jboss.tools.common.el.core.elSearcher"; //$NON-NLS-1$

	String id;
	ELSearcher searcher;

	public ELSearcherExtension() {}

	public String getId() {
		return id;
	}

	public ELSearcher getELSearcher() {
		return searcher;
	}

	static ELSearcherExtension[] INSTANCES;

	public static ELSearcherExtension[] getInstances() {
		if(INSTANCES != null) return INSTANCES;
		List<ELSearcherExtension> list = new ArrayList<ELSearcherExtension>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		IConfigurationElement[] es = point.getConfigurationElements();
		for (IConfigurationElement e: es) {
			ELSearcherExtension n = new ELSearcherExtension();
			n.id = e.getAttribute("id"); //$NON-NLS-1$
			try{
				n.searcher = (ELSearcher)e.createExecutableExtension("searcher-class"); //$NON-NLS-1$
			}catch(CoreException ex){
				Activator.getDefault().logError(ex);
			}
			list.add(n);
		}
		return INSTANCES = list.toArray(new ELSearcherExtension[0]);
	}
}
