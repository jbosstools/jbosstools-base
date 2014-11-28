package org.jboss.tools.tests.tests;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.equinox.p2.core.IProvisioningAgent;
import org.eclipse.equinox.p2.repository.metadata.IMetadataRepositoryManager;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

public class Activator extends Plugin {


    private static Activator plugin;

    private IProvisioningAgent agent;

    private ServiceReference<IProvisioningAgent> agentReference;

    public Activator()
    {
    }

    public void start( BundleContext context )
        throws Exception
    {
        super.start( context );
        plugin = this;
    }

    public void stop( BundleContext context )
        throws Exception
    {
        if ( agentReference != null )
        {
            getBundle().getBundleContext().ungetService( agentReference );
        }

        plugin = null;
        super.stop( context );
    }

    public static Activator getDefault()
    {
        return plugin;
        
    }


    public synchronized IProvisioningAgent getProvisioningAgent()
    {
        if ( agent == null )
        {
            BundleContext bundleContext = getBundle().getBundleContext();
            agentReference = bundleContext.getServiceReference( IProvisioningAgent.class );

            if ( agentReference == null )
            {
                throw new IllegalStateException();
            }

            agent = bundleContext.getService( agentReference );

            if ( agent == null )
            {
                throw new IllegalStateException();
            }
        }
        return agent;
    }

    public static IMetadataRepositoryManager getRepositoryManager()
    {
        IProvisioningAgent agent = getDefault().getProvisioningAgent();

        IMetadataRepositoryManager repoMgr =
            (IMetadataRepositoryManager) agent.getService( IMetadataRepositoryManager.SERVICE_NAME );

        if ( repoMgr == null )
        {
            throw new IllegalStateException();
        }

        return repoMgr;
    }


}
