return {
  'folke/which-key.nvim',
  event = 'VimEnter',
  opts = {
    preset = 'helix',
    icons = {
      mappings = true,
      keys = {},
    },
    spec = {
      { '<leader>c', group = '[C]ode', mode = { 'n', 'x' } },
      { '<leader>d', group = '[D]ocument' },
      { '<leader>g', group = '[G]it' },
      { '<leader>gh', group = '[H]unk' },
      { '<leader>r', group = '[R]ename' },
      { '<leader>s', group = '[S]earch' },
      { '<leader>w', group = '[W]orkspace' },
      { '<leader>t', group = '[T]oggle' },
    }
  }
}
