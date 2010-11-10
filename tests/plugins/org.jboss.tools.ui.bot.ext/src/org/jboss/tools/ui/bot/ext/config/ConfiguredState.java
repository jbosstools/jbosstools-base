package org.jboss.tools.ui.bot.ext.config;

import java.util.List;
import java.util.Vector;

import org.jboss.tools.ui.bot.ext.config.requirement.RequirementBase;

/**
 * this class represents state of running test suite. Properties of this object
 * should be changed only by classed extending {@link RequirementBase} class.
 * 
 * @author lzoubek@redhat.com
 * 
 */
public class ConfiguredState {
	private List<String> jreList = new Vector<String>();
	private Server server = new Server();
	private Seam seam = new Seam();
	private ESB esb = new ESB();
	private JBPM jbpm = new JBPM();
	private DB db = new DB();

	private boolean viewsPrepared = false;

	public boolean isViewsPrepared() {
		return viewsPrepared;
	}

	public void setViewsPrepared(boolean viewsPrepared) {
		this.viewsPrepared = viewsPrepared;
	}

	/**
	 * gets list of installed jre's (without the default one)
	 * 
	 * @return
	 */
	public List<String> getJreList() {
		return jreList;
	}

	/**
	 * gets configured seam runtime
	 * 
	 * @return
	 */
	public Seam getSeam() {
		return seam;
	}

	/**
	 * gets configured server state
	 * 
	 * @return
	 */
	public Server getServer() {
		return server;
	}

	/**
	 * gets configured esb
	 * 
	 * @return
	 */
	public ESB getEsb() {
		return esb;
	}

	/**
	 * gets configured jbpm
	 */
	public JBPM getJBPM() {
		return jbpm;
	}
	
	/**
	 * gets configured database
	 */
	public DB getDB() {
		return db;
	}
	
	public class Server {
		/**
		 * is server runtime & server added?
		 */
		public boolean isConfigured = false;
		/**
		 * is server running?
		 */
		public boolean isRunning = false;
		/**
		 * version of server
		 */
		public String version = null;

		/**
		 * type (EAP | JbossAS )
		 */
		public String type = null;
		/**
		 * name of added server/runtime
		 */
		public String name = null;
		/**
		 * version of java configured to server (1.5 or 1.6)
		 */
		public String withJavaVersion = null;
		/**
		 * version of bundled ESB (applicable only for server type SOA)
		 */
		public String bundledESBVersion=null;
	}

	public class Seam {
		/**
		 * version of seam runtime
		 */
		public String version = null;
		/**
		 * is configured?
		 */
		public boolean isConfiured = false;
		/**
		 * name of added runtime
		 */
		public String name = null;
	}

	public class ESB {
		/**
		 * version of ESB runtime
		 */
		public String version = null;
		/**
		 * is configured?
		 */
		public boolean isConfiured = false;
		/**
		 * name of added runtime
		 */
		public String name = null;
	}
	
	public class JBPM {
		/**
		 * version of ESB runtime
		 */		
		public String version = null;
		/**
		 * is configured?
		 */		
		public boolean isConfigured = false;
		/**
		 * name of added runtime
		 */		
		public String name = null;
	}
	
	
	public class DB {		
		/**
		 * version of DB
		 */		
		public String version = null;
		/**
		 * is configured?
		 */		
		public boolean isConfigured = false;
		/**
		 * name of added runtime
		 */		
		public String name = null;
	}
}
